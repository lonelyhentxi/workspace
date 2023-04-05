use swc_core::ecma::{
    ast::*,
    visit::{Fold, noop_fold_type, FoldWith},
};
use swc_core::plugin::{plugin_transform, proxies::TransformPluginProgramMetadata};
use swc_core::quote;
use swc_common::{ DUMMY_SP };

pub struct SwcPluginTest {
    script_stmt_idx: usize,
    module_item_idx: usize
}

impl SwcPluginTest {
}

impl Fold for SwcPluginTest {
    noop_fold_type!();

    fn fold_script (&mut self, mut script: Script) -> Script {
        script.body = script.body.into_iter().enumerate().map(|(idx, stmt)| {
            self.script_stmt_idx = idx;
            stmt.fold_with(self)
        }).collect();
        script
    }

    fn fold_module (&mut self, mut module: Module) -> Module {
        module.body = module.body.into_iter().enumerate().map(|(idx, item)| {
            self.module_item_idx = idx;
            item.fold_with(self)
        }).collect();
        module
    }
}

fn swc_plugin_test () -> SwcPluginTest {
    SwcPluginTest {
        script_stmt_idx: 0,
        module_item_idx: 0,
    }
}

#[plugin_transform]
pub fn process_transform(program: Program, _metadata: TransformPluginProgramMetadata) -> Program {
    let mut p = swc_plugin_test();
    program.fold_with(&mut p)
}

#[cfg(test)]
mod tests {
    use super::*;
    use swc_core::ecma::transforms::testing::test;

    // An example to test plugin transform.
    // Recommended strategy to test plugin's transform is verify
    // the Visitor's behavior, instead of trying to run `process_transform` with mocks
    // unless explicitly required to do so.
    test!(
        Default::default(),
        |_| swc_plugin_test(),
        boo,
        // Input codes
        r#"console.log("transform");"#,
        // Output codes after transformed with plugin
        r#"console.log("transform");"#
    );
}