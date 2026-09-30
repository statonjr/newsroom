# newsroom

Newsroom reads the day's news from a bunch of RSS feeds and web searches, then hands the stories to an LLM that writes a briefing about them. The briefing covers politics, economics, and science and tech, but what it really cares about is how those push on each other. When tariffs go up, or bond yields climb, or a lab in one country pulls ahead, the briefing tries to trace what happens next. Every claim links back to the story it came from, so you can always check what the model is telling you.

The sources lean global on purpose. Western outlets sit next to Al Jazeera, CGTN, Global Times, Xinhua and a few others, since a single set of papers tends to tell a single story.

It's written in Clojure and runs on [jolt](https://github.com/jolt-lang/jolt).

## Running it

The [releases](https://github.com/yogthos/newsroom/releases) page has builds for macOS, Linux and Windows, so you can grab one of those and run `newsroom` without installing anything else. From a checkout you'd run it with jolt instead.

```
jolt serve
```

Open http://127.0.0.1:3000 and you'll get a page for today with the archive in the sidebar. On the first start it writes a default setup to `~/.config/newsroom`, and if today doesn't have a briefing yet it goes and gathers one right away. You can watch that happen, since the sidebar shows each feed it reads and each search it runs while the model thinks and writes. After that it runs every morning at the time you set, and the button in the sidebar gets you a fresh one whenever you want.

The model needs an API key. DeepSeek is the default and reads `DEEPSEEK_API_KEY`, though GLM, OpenAI, Ollama and a local llama.cpp server work just as well.

## Making it yours

Everything you'd want to change lives in `~/.config/newsroom`. The sources, schedule and model are all in `config.edn`, while `prompt.md` holds the instructions the model gets, so that's the file to edit when you want a different kind of briefing. A source can be an RSS feed or a web search, and a site with no feed can still be read by scraping the story links off its front page. [`examples/config.edn`](examples/config.edn) walks through every kind of source and every model provider. To follow a source newsroom doesn't know about, drop a `.clj` file into `plugins/` that teaches it a new source type.

Each day goes into a sqlite database and gets written out as a markdown file in `briefings/` too. Only the last 100 days are kept, which stops a long-running server from slowly eating the disk, and you can set `:keep-days` to -1 if you'd rather keep everything.

## Tests

```
jolt test
```

That runs the test suite and then checks the pure core against its [writ](https://github.com/jlt-commons/writ) spec in `test/newsroom/news_spec.clj`.
