use leptos::{
    ev::MouseEvent, portal::Portal, prelude::*, server::codee::string::FromToStringCodec,
};
use leptos_use::{storage::use_local_storage, use_debounce_fn};

use crate::interpreter::run_code;

mod docs;
use docs::DOCS;

#[component]
#[allow(non_snake_case)]
pub fn App() -> impl IntoView {
    let (read_stored, write_stored, _) =
        use_local_storage::<String, FromToStringCodec>("code-store");

    let (code, set_code) = signal(read_stored.get_untracked());
    let (output, set_output) = signal("".to_string());

    // Write code to local storage 500ms after last key pressed
    let debounce = use_debounce_fn(
        move || {
            write_stored.set(code.get());
        },
        500.0,
    );

    let run_code = move |_: MouseEvent| {
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
    };

    view! {
        <div class:container>
            <div class:sidebar>
                <SidebarContents />
            </div>
            <div class:code-region>
                <textarea
                    class:code-input
                    on:input:target=move |ev| {
                        set_code.set(ev.target().value());
                        debounce();
                    }
                    prop:value=code
                    prop:spellcheck=false
                />
                <br />
                <button on:click=run_code class:run-button>
                    "Run Code"
                </button>
                <pre class:code-output>{output}</pre>
            </div>
        </div>
    }
}

#[component]
#[allow(non_snake_case)]
fn SidebarContents() -> impl IntoView {
    let (show_docs, set_show_docs) = signal(false);

    view! {
        <img class:logo src="/public/omnidice-logo.png" />
        <h1>"OmniDice"</h1>
        <h5>"Dice Calculator That Definitely Isn't Just a Programming Language"</h5>
        <button on:click=move |_| set_show_docs.set(true) class:docs-button>
            "Documentation"
        </button>
        <h6 class:attribution>
            "Made by Nathan Strong, "
            <a prop:href="https://github.com/NathanNull">"or NathanNull on GitHub"</a>
        </h6>

        <Show when=move || show_docs.get() fallback=|| view! {}>
            <Portal>
                <div class:modal-backdrop on:click=move |_| set_show_docs.set(false)>
                    <div class:modal on:click=|ev| ev.stop_propagation()>
                        <div class:docs inner_html=DOCS.as_str() />
                        <button
                            class:docs-button
                            on:click=move |ev| {
                                set_show_docs.set(false);
                                ev.stop_propagation();
                            }
                        >
                            "Close"
                        </button>
                    </div>
                </div>
            </Portal>
        </Show>
    }
}
