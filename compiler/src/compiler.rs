// Copyright (C) 2019-2021 Aleo Systems Inc.
// This file is part of the Leo library.

// The Leo library is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// The Leo library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with the Leo library. If not, see <https://www.gnu.org/licenses/>.

//! Compiles a Leo program from a file path.
use crate::{
    constraints::{generate_constraints, generate_test_constraints},
    AstSnapshotOptions, CompilerOptions, GroupType, Output, OutputFile, TypeInferencePhase,
};
pub use leo_asg::{new_context, AsgContext as Context, AsgContext};
use leo_asg::{Asg, AsgPass, Program as AsgProgram};
use leo_ast::{AstPass, Input, MainInput, Program as AstProgram};
use leo_errors::{emitter::Handler, CompilerError, Result};
use leo_imports::ImportParser;
use leo_input::LeoInputParser;
use leo_package::inputs::InputPairs;
use leo_parser::parse_ast;
use leo_state::verify_local_data_commitment;

use snarkvm_dpc::testnet1::{instantiated::Components, parameters::SystemParameters};
use snarkvm_fields::PrimeField;
use snarkvm_r1cs::{ConstraintSynthesizer, ConstraintSystem, SynthesisError};

use sha2::{Digest, Sha256};
use std::{
    fs,
    marker::PhantomData,
    path::{Path, PathBuf},
};

use indexmap::IndexMap;

thread_local! {
    static THREAD_GLOBAL_CONTEXT: AsgContext<'static> = {
        let leaked = Box::leak(Box::new(leo_asg::new_alloc_context()));
        leo_asg::new_context(leaked)
    }
}

/// Convenience function to return a leaked thread-local global context. Should only be used for transient programs (like cli).
pub fn thread_leaked_context() -> AsgContext<'static> {
    THREAD_GLOBAL_CONTEXT.with(|f| *f)
}

/// Stores information to compile a Leo program.
#[derive(Clone)]
pub struct Compiler<'a, 'b, F: PrimeField, G: GroupType<F>> {
    handler: &'b Handler,
    program_name: String,
    main_file_path: PathBuf,
    output_directory: PathBuf,
    program: AstProgram,
    program_input: Input,
    context: AsgContext<'a>,
    asg: Option<AsgProgram<'a>>,
    options: CompilerOptions,
    imports_map: IndexMap<String, String>,
    ast_snapshot_options: AstSnapshotOptions,
    _engine: PhantomData<F>,
    _group: PhantomData<G>,
}

impl<'a, 'b, F: PrimeField, G: GroupType<F>> Compiler<'a, 'b, F, G> {
    /// Returns a new Leo program compiler.
    pub fn new(
        handler: &'b Handler,
        package_name: String,
        main_file_path: PathBuf,
        output_directory: PathBuf,
        context: AsgContext<'a>,
        options: Option<CompilerOptions>,
        imports_map: IndexMap<String, String>,
        ast_snapshot_options: Option<AstSnapshotOptions>,
    ) -> Self {
        Self {
            handler,
            program_name: package_name.clone(),
            main_file_path,
            output_directory,
            program: AstProgram::new(package_name),
            program_input: Input::new(),
            asg: None,
            context,
            options: options.unwrap_or_default(),
            imports_map,
            ast_snapshot_options: ast_snapshot_options.unwrap_or_default(),
            _engine: PhantomData,
            _group: PhantomData,
        }
    }

    /// Returns a new `Compiler` from the given main file path.
    ///
    /// Parses and stores a program from the main file path.
    /// Parses and stores all imported programs.
    /// Performs type inference checking on the program and imported programs.
    ///
    pub fn parse_program_without_input(
        handler: &'b Handler,
        package_name: String,
        main_file_path: PathBuf,
        output_directory: PathBuf,
        context: AsgContext<'a>,
        options: Option<CompilerOptions>,
        imports_map: IndexMap<String, String>,
        ast_snapshot_options: Option<AstSnapshotOptions>,
    ) -> Result<Self> {
        let mut compiler = Self::new(
            handler,
            package_name,
            main_file_path,
            output_directory,
            context,
            options,
            imports_map,
            ast_snapshot_options,
        );

        compiler.parse_program()?;

        Ok(compiler)
    }

    pub fn set_options(&mut self, options: CompilerOptions) {
        self.options = options;
    }

    /// Returns a new `Compiler` from the given main file path.
    ///
    /// Parses and stores program input from from the input file path and state file path
    /// Parses and stores a program from the main file path.
    /// Parses and stores all imported programs.
    /// Performs type inference checking on the program, imported programs, and program input.
    #[allow(clippy::too_many_arguments)]
    pub fn parse_program_with_input(
        handler: &'b Handler,
        package_name: String,
        main_file_path: PathBuf,
        output_directory: PathBuf,
        input_string: &str,
        input_path: &Path,
        state_string: &str,
        state_path: &Path,
        context: AsgContext<'a>,
        options: Option<CompilerOptions>,
        imports_map: IndexMap<String, String>,
        ast_snapshot_options: Option<AstSnapshotOptions>,
    ) -> Result<Self> {
        let mut compiler = Self::new(
            handler,
            package_name,
            main_file_path,
            output_directory,
            context,
            options,
            imports_map,
            ast_snapshot_options,
        );

        compiler.parse_input(input_string, input_path, state_string, state_path)?;

        compiler.parse_program()?;

        Ok(compiler)
    }

    /// Parses and stores program input from from the input file path and state file path
    ///
    /// Calls `set_path()` on compiler errors with the given input file path or state file path
    pub fn parse_input(
        &mut self,
        input_string: &str,
        input_path: &Path,
        state_string: &str,
        state_path: &Path,
    ) -> Result<()> {
        let input_syntax_tree = LeoInputParser::parse_file(input_string).map_err(|mut e| {
            e.set_path(
                input_path.to_str().unwrap_or_default(),
                &input_string.lines().map(|x| x.to_string()).collect::<Vec<String>>()[..],
            );

            e
        })?;
        let state_syntax_tree = LeoInputParser::parse_file(state_string).map_err(|mut e| {
            e.set_path(
                state_path.to_str().unwrap_or_default(),
                &state_string.lines().map(|x| x.to_string()).collect::<Vec<String>>()[..],
            );

            e
        })?;

        self.program_input.parse_input(input_syntax_tree).map_err(|mut e| {
            e.set_path(
                input_path.to_str().unwrap_or_default(),
                &input_string.lines().map(|x| x.to_string()).collect::<Vec<String>>()[..],
            );

            e
        })?;
        self.program_input.parse_state(state_syntax_tree).map_err(|mut e| {
            e.set_path(
                state_path.to_str().unwrap_or_default(),
                &state_string.lines().map(|x| x.to_string()).collect::<Vec<String>>()[..],
            );

            e
        })?;

        Ok(())
    }

    /// Parses and stores the main program file, constructs a syntax tree, and generates a program.
    ///
    /// Parses and stores all programs imported by the main program file.
    pub fn parse_program(&mut self) -> Result<()> {
        // Load the program file.
        let content = fs::read_to_string(&self.main_file_path)
            .map_err(|e| CompilerError::file_read_error(self.main_file_path.clone(), e))?;

        self.parse_program_from_string(&content)
    }

    /// Equivalent to parse_and_check_program but uses the given program_string instead of a main
    /// file path.
    pub fn parse_program_from_string(&mut self, program_string: &str) -> Result<()> {
        // Use the parser to construct the abstract syntax tree (ast).

        let mut ast: leo_ast::Ast = parse_ast(
            self.handler,
            self.main_file_path.to_str().unwrap_or_default(),
            program_string,
        )?;

        if self.ast_snapshot_options.initial {
            if self.ast_snapshot_options.spans_enabled {
                ast.to_json_file(self.output_directory.clone(), "initial_ast.json")?;
            } else {
                ast.to_json_file_without_keys(self.output_directory.clone(), "initial_ast.json", &["span"])?;
            }
        }

        // Preform import resolution.
        ast = leo_ast_passes::Importer::do_pass(
            self.handler,
            ast.into_repr(),
            &mut ImportParser::new(self.handler, self.main_file_path.clone(), self.imports_map.clone()),
        )?;

        if self.ast_snapshot_options.imports_resolved {
            if self.ast_snapshot_options.spans_enabled {
                ast.to_json_file(self.output_directory.clone(), "imports_resolved_ast.json")?;
            } else {
                ast.to_json_file_without_keys(self.output_directory.clone(), "imports_resolved_ast.json", &["span"])?;
            }
        }

        // Preform canonicalization of AST always.
        ast = leo_ast_passes::Canonicalizer::do_pass(ast.into_repr())?;

        if self.ast_snapshot_options.canonicalized {
            if self.ast_snapshot_options.spans_enabled {
                ast.to_json_file(self.output_directory.clone(), "canonicalization_ast.json")?;
            } else {
                ast.to_json_file_without_keys(self.output_directory.clone(), "canonicalization_ast.json", &["span"])?;
            }
        }

        // Store the main program file.
        self.program = ast.into_repr();
        self.program.name = self.program_name.clone();

        tracing::debug!("Program parsing complete\n{:#?}", self.program);

        // Create a new symbol table from the program, imported_programs, and program_input.
        let asg = Asg::new(self.context, &self.program)?;

        if self.ast_snapshot_options.type_inferenced {
            let new_ast = TypeInferencePhase::default()
                .phase_ast(&self.program, &asg.clone().into_repr())
                .expect("Failed to produce type inference ast.");

            if self.ast_snapshot_options.spans_enabled {
                new_ast.to_json_file(self.output_directory.clone(), "type_inferenced_ast.json")?;
            } else {
                new_ast.to_json_file_without_keys(
                    self.output_directory.clone(),
                    "type_inferenced_ast.json",
                    &["span"],
                )?;
            }
        }

        tracing::debug!("ASG generation complete");

        // Store the ASG.
        self.asg = Some(asg.into_repr());

        self.do_asg_passes()?;

        Ok(())
    }

    /// Run compiler optimization passes on the program in asg format.
    fn do_asg_passes(&mut self) -> Result<()> {
        assert!(self.asg.is_some());

        // Do constant folding.
        if self.options.constant_folding_enabled {
            let asg = self.asg.take().unwrap();
            self.asg = Some(leo_asg_passes::ConstantFolding::do_pass(asg)?);
        }

        // Do dead code elimination.
        if self.options.dead_code_elimination_enabled {
            let asg = self.asg.take().unwrap();
            self.asg = Some(leo_asg_passes::DeadCodeElimination::do_pass(asg)?);
        }

        Ok(())
    }

    /// Synthesizes the circuit with program input to verify correctness.
    pub fn compile_constraints<CS: ConstraintSystem<F>>(&self, cs: &mut CS) -> Result<Output> {
        generate_constraints::<F, G, CS>(cs, self.asg.as_ref().unwrap(), &self.program_input)
    }

    /// Synthesizes the circuit for test functions with program input.
    pub fn compile_test_constraints(self, input_pairs: InputPairs) -> Result<(u32, u32)> {
        generate_test_constraints::<F, G>(self.asg.as_ref().unwrap(), input_pairs, &self.output_directory)
    }

    /// Returns a SHA256 checksum of the program file.
    pub fn checksum(&self) -> Result<String> {
        // Read in the main file as string
        let unparsed_file = fs::read_to_string(&self.main_file_path)
            .map_err(|e| CompilerError::file_read_error(self.main_file_path.clone(), e))?;

        // Hash the file contents
        let mut hasher = Sha256::new();
        hasher.update(unparsed_file.as_bytes());
        let hash = hasher.finalize();

        Ok(format!("{:x}", hash))
    }

    /// TODO (howardwu): Incorporate this for real program executions and intentionally-real
    ///  test executions. Exclude it for test executions on dummy data.
    ///
    /// Verifies the input to the program.
    pub fn verify_local_data_commitment(&self, system_parameters: &SystemParameters<Components>) -> Result<bool> {
        let result = verify_local_data_commitment(system_parameters, &self.program_input)?;

        Ok(result)
    }

    /// Manually sets main function input.
    ///
    /// Used for testing only.
    pub fn set_main_input(&mut self, input: MainInput) {
        self.program_input.set_main_input(input);
    }
}

impl<F: PrimeField, G: GroupType<F>> ConstraintSynthesizer<F> for Compiler<'_, '_, F, G> {
    /// Synthesizes the circuit with program input.
    fn generate_constraints<CS: ConstraintSystem<F>>(&self, cs: &mut CS) -> Result<(), SynthesisError> {
        let output_directory = self.output_directory.clone();
        let package_name = self.program_name.clone();

        let result = match self.compile_constraints(cs) {
            Err(err) => {
                eprintln!("{}", err);
                std::process::exit(err.exit_code())
            }
            Ok(result) => result,
        };

        // Write results to file
        let output_file = OutputFile::new(&package_name);
        output_file
            .write(&output_directory, result.to_string().as_bytes())
            .unwrap();

        Ok(())
    }
}
