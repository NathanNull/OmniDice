use leptos::{
    ev::MouseEvent, portal::Portal, prelude::*, server::codee::string::FromToStringCodec,
};
use leptos_use::{storage::use_local_storage, use_debounce_fn};
use web_sys::{
    MessageEvent, Worker, WorkerOptions, WorkerType,
    js_sys::Array,
    wasm_bindgen::{JsCast, prelude::Closure},
    window,
};

mod docs;
use docs::DOCS;

fn worker_new(url: &str) -> Worker {
    let options = WorkerOptions::new();
    options.set_type(WorkerType::Module);
    Worker::new_with_options(&url, &options).expect("failed to spawn worker")
}

#[derive(PartialEq, Clone)]
enum WorkerState {
    Loading,
    Ready,
    Running,
}

#[component]
#[allow(non_snake_case)]
pub fn App() -> impl IntoView {
    let (read_stored, write_stored, _) =
        use_local_storage::<String, FromToStringCodec>("code-store");

    let (code, set_code) = signal(read_stored.get_untracked());
    let (output, set_output) = signal("Code Output".to_string());

    let (worker_state, set_worker_state) = signal(WorkerState::Loading);

    let worker = worker_new("./worker_loader.js");
    //let worker_clone = worker.clone();

    let onmessage = Closure::wrap(Box::new(move |msg: MessageEvent| {
        //let worker_clone = worker_clone.clone();
        let data = Array::from(&msg.data());
        match data
            .get(0)
            .as_string()
            .expect("first field is string")
            .as_str()
        {
            "ready" => {
                set_worker_state.set(WorkerState::Ready)
            }
            "result" => {
                let out = data
                    .get(1)
                    .as_string()
                    .expect("result second field is string");
                *set_output.write() += &out;
            }
            _ => (),
        }
    }) as Box<dyn Fn(MessageEvent)>);

    worker.set_onmessage(Some(onmessage.as_ref().unchecked_ref()));
    onmessage.forget();

    // Write code to local storage 500ms after last key pressed
    let debounce = use_debounce_fn(
        move || {
            write_stored.set(code.get());
        },
        500.0,
    );

    let run_code = move |_: MouseEvent| {
        if worker_state.get() == WorkerState::Ready {
            set_output.set(String::new());
            set_worker_state.set(WorkerState::Running);
            worker
                .post_message(&code.get().into())
                .expect("post code to work");
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
                <button
                    on:click=run_code
                    class:run-button
                    disabled=move || worker_state.get() != WorkerState::Ready
                >
                    "Run Code"
                </button>
                <pre class:code-output>
                    {move || {
                        let out = output.get();
                        if out.len() == 0 { "Thinking...".to_string() } else { out }
                    }}
                </pre>
            </div>
        </div>
    }
}

#[component]
#[allow(non_snake_case)]
fn SidebarContents() -> impl IntoView {
    let location = window().map(|w| w.location());
    let url = location.as_ref().and_then(|l| l.href().ok()).unwrap_or(String::new());
    let (show_docs, set_show_docs) = signal(false);

    view! {
        <img class:logo src={move || format!("{url}/public/omnidice-logo.png")} />
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
