## the erlang re-up

**Watches** your erlang source/header files for changes, **recompiles** the
changed modules using the same compiler options as the last compile, and
**reloads** them.

Conceptually similar to [sync](https://github.com/rustyio/sync) and [active](https://github.com/synrc/active), but works happily on NFS or vbox shared folders. Stateless, because we watch the filesystem by
polling with find, so can handle enormous erlang projects just fine.

Because it reuses the compile options from last time (provided you
compile with `debug_info`), it will Just Work, regardless of what you use to build your project (rebar2, rebar3, make, etc).

### Building

Add this application as a dev-only dependency to your project. If you're
using rebar3, which I recommend, this should be easy.

Something, something..

    application:start(reup).

### Options

If you want reup to compile but not load the modules, set application env:

`{reload_on_compile, false}`


### Implementation Notes

Has to work on virtualbox with crappy shared folders, so doesn't use
inotify or fswatcher stuff. Known to work on Ubuntu Linux & OS X.

The script in `priv/` polls for src changes using some shell gubbins,
and emits changed filenames.

If a `*.hrl` file changes, it naively emits the filenames of `*.erl`
files that match a grep for the header filename, which tends to work.
