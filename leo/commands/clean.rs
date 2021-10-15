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

use crate::{commands::Command, context::Context};
use leo_compiler::OutputFile;
use leo_errors::Result;
use leo_package::outputs::{
    ChecksumFile, CircuitFile, ProofFile, ProvingKeyFile, Snapshot, SnapshotFile, VerificationKeyFile,
};

use structopt::StructOpt;
use tracing::span::Span;

/// Clean outputs folder command
#[derive(StructOpt, Debug)]
#[structopt(setting = structopt::clap::AppSettings::ColoredHelp)]
pub struct Clean {}

impl<'a> Command<'a> for Clean {
    type Input = ();
    type Output = ();

    fn log_span(&self) -> Span {
        tracing::span!(tracing::Level::INFO, "Cleaning")
    }

    fn prelude(&self, _: Context<'a>) -> Result<Self::Input> {
        Ok(())
    }

    fn apply(self, context: Context<'a>, _: Self::Input) -> Result<Self::Output> {
        let path = context.dir()?;
        let package_name = context.manifest()?.get_package_name();

        // Remove the checksum from the output directory
        ChecksumFile::new(&package_name).remove(&path)?;

        // Remove the serialized circuit from the output directory
        CircuitFile::new(&package_name).remove(&path)?;

        // Remove the program output file from the output directory
        OutputFile::new(&package_name).remove(&path)?;

        // Remove the proving key from the output directory
        ProvingKeyFile::new(&package_name).remove(&path)?;

        // Remove the verification key from the output directory
        VerificationKeyFile::new(&package_name).remove(&path)?;

        // Remove the proof from the output directory
        ProofFile::new(&package_name).remove(&path)?;

        // Remove AST snapshots from the output directory
        SnapshotFile::new(&package_name, Snapshot::Initial).remove(&path)?;
        SnapshotFile::new(&package_name, Snapshot::ImportsResolved).remove(&path)?;
        SnapshotFile::new(&package_name, Snapshot::TypeInference).remove(&path)?;
        SnapshotFile::new(&package_name, Snapshot::Canonicalization).remove(&path)?;

        Ok(())
    }
}
