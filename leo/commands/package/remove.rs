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

// !!!!!!!!!!!!!!!!!!!!!!!!!!!!
// COMMAND TEMPORARILY DISABLED
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!

use crate::{commands::Command, context::Context};
use leo_errors::Result;
use leo_package::LeoPackage;

use structopt::StructOpt;
use tracing::span::Span;

/// Remove imported package
#[derive(StructOpt, Debug)]
#[structopt(setting = structopt::clap::AppSettings::ColoredHelp)]
pub struct Remove {
    #[structopt(name = "PACKAGE")]
    name: String,
}

impl<'a> Command<'a> for Remove {
    type Input = ();
    type Output = ();

    fn log_span(&self) -> Span {
        tracing::span!(tracing::Level::INFO, "Removing")
    }

    fn prelude(&self, _: Context<'a>) -> Result<Self::Input> {
        Ok(())
    }

    fn apply(self, context: Context<'a>, _: Self::Input) -> Result<Self::Output> {
        let path = context.dir()?;
        let package_name = self.name;

        LeoPackage::remove_imported_package(&package_name, &path)?;
        tracing::info!("Successfully removed package \"{}\"\n", package_name);

        Ok(())
    }
}
