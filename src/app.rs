use leptos::{prelude::*, server::codee::string::FromToStringCodec};
use leptos_use::{storage::use_local_storage, use_debounce_fn};

use crate::interpreter::run_code;

#[component]
#[allow(non_snake_case)]
pub fn App() -> impl IntoView {
    let (read_stored, write_stored, _) =
        use_local_storage::<String, FromToStringCodec>("code-store");

    let (code, set_code) = signal(read_stored.get_untracked());
    let (output, set_output) = signal("".to_string());

    // Write code to local storage 500ms after last key pressed
    let debounce = use_debounce_fn(move || {
        write_stored.set(code.get());
    }, 500.0);

    view! {
        <div
            style:display="flex"
            style:flex-direction="column"
            style:width="100vw"
            style:height="100vh"
            style:position="absolute"
            style:top="0"
            style:left="0"
            style:padding="5vw"
        >
            <textarea
                class:code-input
                on:input:target=move |ev| {
                    set_code.set(ev.target().value());
                    debounce();
                }
                prop:value=code
                style:height="70%"
                style:width="100%"
                style:resize="none"
                prop:spellcheck=false
            />
            <br />
            <button
                on:click=move |_| {
                    set_output.set(String::new());
                    let res = run_code(
                        &code.get(),
                        None,
                        Box::new(move |out| *set_output.write() += out),
                    );
                    match res {
                        Ok(_) => {}
                        Err(err) => {
                            *set_output.write() += &err.write(&code.get());
                        }
                    }
                }
                style:width="100%"
            >
                "Run Code"
            </button>
            <pre
                style:height="20%"
                style:width="100%"
                style:max-height="20%"
                style:overflow="scroll"
            >
                {output}
            </pre>
        </div>
    }
}
