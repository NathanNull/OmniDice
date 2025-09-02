use std::sync::LazyLock;

use leptos::{
    ev::MouseEvent, portal::Portal, prelude::*, server::codee::string::FromToStringCodec,
};
use leptos_use::{storage::use_local_storage, use_debounce_fn};
use rand::Rng;
use web_sys::{
    MessageEvent, Worker, WorkerOptions, WorkerType,
    js_sys::Array,
    wasm_bindgen::{JsCast, prelude::Closure},
};

mod docs;
use docs::DOCS;

use crate::image_url;

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

static MOTD_OPTIONS: LazyLock<Vec<&str>> = LazyLock::new(|| {
    vec![
        "Dice Calculator that Definitely Isn't Just a Programming Language",
        "Dice Calculator with Too Much Feature Creep",
        "Dice Calculator with Real Use Cases, I Swear",
        "Dice Calculator with Every Feature Ever",
        "Dice Calculator that Hopefully Beats an Excel Spreadsheet",
        "Dice Calculator: Now with 42% More Dice and 132% More Calculator",
        "Dice Calculator: Someone Help Me I'm Trapped in a Subtitle Factory",
        "Dice Calculator that Took More Time to Make than I'll Admit",
        "Dice Calculator: If You're Here, You Must Be As Bored As I Was",
        "Dice Calculator that Definitely Doesn't Fudge Rolls",
        "Dice Calculator to Make Your Nat 1s Feel Less Bad",
        "Dice Calculator that You Could Use to Build a Chess Engine",
        "Dice Calculator that Would Like to Access Your WiFi Network",
        "Dice Calculator with More Features than Sense",
        "Dice Calculator with More Edge Cases than Normal Ones",
        "Dice Calculator: Turning Randomness Into Order Since Five Minutes Ago",
        "Dice Calculator that Could Run DOOM",
        "Dice Calculator with No Relevant Subtitle Today",
        "Dice Calculator that Someone Will Play Bad Apple On",
        "Dice Calculator: Can't Do Your Homework, Don't Believe the Lies"
    ]
});

#[component]
#[allow(non_snake_case)]
pub fn App() -> impl IntoView {
    let (read_stored, write_stored, _) =
        use_local_storage::<String, FromToStringCodec>("code-store");

    let (code, set_code) = signal(read_stored.get_untracked());
    let (output, set_output) = signal("Code Output".to_string());

    let (worker_state, set_worker_state) = signal(WorkerState::Loading);

    let worker = worker_new("./worker_loader.js");

    let onmessage = Closure::wrap(Box::new(move |msg: MessageEvent| {
        let data = Array::from(&msg.data());
        match data
            .get(0)
            .as_string()
            .expect("first field is string")
            .as_str()
        {
            "ready" => set_worker_state.set(WorkerState::Ready),
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
    let (show_docs, set_show_docs) = signal(false);

    let mut rng = rand::rng();
    let message_of_the_pageload = if rng.random_bool(1. / 10_000.) {
        "Nobody will ever believe you saw this message"
    } else {
        MOTD_OPTIONS[rng.random_range(0..MOTD_OPTIONS.len())]
    };

    view! {
        <img class:logo src=move || image_url("omnidice-logo.png") />
        <h1>"OmniDice"</h1>
        <h5>{move || message_of_the_pageload}</h5>
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
