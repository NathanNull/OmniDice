use leptos::prelude::*;

use crate::interpreter::run_code;

#[component]
pub fn App() -> impl IntoView {
    let (code, set_code) = signal("".to_string());
    let (output, set_output) = signal("".to_string());

    view! {
        <div
            style:width="100vw"
            style:height="100vh"
            style:display="flex"
            style:position="absolute"
            style:top="0"
            style:left="0"
            style:flex-direction="column"
            style:padding="5vw"
        >
            <textarea
                class:code-input
                on:input:target=move |ev| {
                    set_code.set(ev.target().value());
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
            <pre style:height="20%" style:width="100%" style:max-height="20%" style:overflow="scroll">{output}</pre>
        </div>
    }
}
