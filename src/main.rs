use boa_engine::NativeFunction;
use boa_engine::class::{Class, ClassBuilder};
use boa_engine::embed_module;
use boa_engine::object::builtins::JsPromise;
use boa_engine::property::Attribute;
use boa_engine::{Context, JsData, JsResult, JsString, JsValue, Source, js_string};
use boa_gc::{Finalize, Trace};
use boa_macros::boa_class;
use boa_macros::boa_module;
use std::rc::Rc;
use std::time::Duration;

fn main() -> JsResult<()> {
    #[cfg(target_family = "unix")]
    let module_loader = Rc::new(embed_module!("dist/", compress = "none"));
    #[cfg(target_family = "windows")]
    let module_loader = Rc::new(embed_module!("dist\\"));

    let js_code = r#"
      import("/index.mjs").then(({main}) => main()).catch(console.log)
  "#;

    // Parse the source code
    let mut context = Context::builder()
        .module_loader(module_loader.clone())
        .build()
        .unwrap();

    let fetch_data = NativeFunction::from_copy_closure(|_, args, context| {
        let user_id = "asdfasdf"; // args.get_or_undefined(0).to_string(context)?;

        // 2. Wrap your async logic in a Promise
        let promise = Promise::new_job(
            context,
            Rc::new(RefCell::new(move |context| {
                // Simulate an async operation or network request here
                let result = format!("Data for user: {}", user_id);
                Ok(JsValue::from(result))
            })),
        );

        Ok(JsValue::from(promise))
    });

    // 3. Register the function in the global scope
    context.register_global_property(
        "fetchData",
        fetch_data,
        boa_engine::property::Attribute::all(),
    )?;

    let _ = boa_runtime::register(
        (
            boa_runtime::extensions::ConsoleExtension::default(),
            boa_runtime::extensions::FetchExtension(
                boa_runtime::fetch::BlockingReqwestFetcher::default(),
            ),
        ),
        None,
        &mut context,
    );

    let result = context.eval(Source::from_bytes(js_code))?;
    let p = result.as_promise().expect("not a promise");
    p.await_blocking(&mut context)
        .expect("error awaiting promise");

    println!("{}", result.display());

    Ok(())
}
