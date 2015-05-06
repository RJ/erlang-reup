## the erlang re-up

**Watches** your erlang source/header files for changes, **recompiles** the
changed modules using the same compiler options as the last compile, and
**reloads** them.

Similar to [sync](https://github.com/rustyio/sync), but with different
bugs :)

Because it reuses the compile options from last time (provided you
compile with `debug_info`), it will Just Work.

### Building

Add this application as a dev-only dependency to your project. If you're
using rebar3, which I recommend, this should be easy.

Something, something..

    application:start(reup).


### Implementation Notes

Has to work on virtualbox with crappy shared folders, so doesn't use
inotify. Only tested on linux. Probably works on mac, if find works the same.

The script in `priv/` polls for src changes using some shell gubbins,
and emits changed filenames.

If a `*.hrl` file changes, it naively emits the filenames of `*.erl`
files that match a grep for the header filename, which tends to work.
