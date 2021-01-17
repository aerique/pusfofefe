# Pusfofefe

A Pushover client written in Common Lisp.

Thanks to [Renaud Casenave-Péré](https://openrepos.net/user/856/programs)
for packaging both ECL and EQL5.

The canonical home page of this project is https://git.sr.ht/~aerique/pusfofefe

(This project is also pushed to GitLab and GitHub but those sites are
not monitored for support.)

## Known Issues

- Having two remorse actions running at the same time will result in
  only one being executed (and an error on STDOUT when running from the
  CLI).
- When running from the CLI you'll see `[W] unknown:-1 - <Unknown File>:
  Syntax error` when downloading messages or logging in.  It seems to be
  connected to spawning threads in ECL and even happens when the
  `download-messages-thread` or `login-and-register-thread` functions
  are empty.  When these functions are not called the error does not
  appear.  However, it does not seems to affect the running of those
  functions.  I have not investigated further.
- No Pushover websocket / "push" support.  So when there are new
  messages one Pushover server it can take up to the refresh time
  defined in settings before they're retrieved by the Pusfofefe app.

## Backup

There's two files if you need to backup your data, they're defined in
the code by `QStandardPaths::AppConfigLocation` and
`QStandardPaths::AppDataLocation` and on Linux usually point to:

- `$HOME/.config/pusfofefe/`
- `$HOME/.local/share/pusfofefe/`

You can replace these files if the program is not running.

## Build

### Dependencies

- [SailfishOS Builds in Docker](https://git.sr.ht/~aerique/sfosbid)
- [Cloverlover](https://git.sr.ht/~aerique/cloverlover)

### Instructions

The project was build using
[SailfishOS Builds in Docker](https://git.sr.ht/~aerique/sfosbid).

Refer to that project's README on how to get it running.

The actual build steps for Pusfofefe are:

- run `sfosbid` Docker container
- `sb2 -t SailfishOS-latest-armv7hl -m sdk-install -R`
    - `rpm --import https://sailfish.openrepos.net/openrepos.key`
    - `zypper ar -f https://sailfish.openrepos.net/razcampagne/personal-main.repo`
    - `zypper in eql5`
    - `exit`
- `git clone https://git.sr.ht/~aerique/pusfofefe`
- `cd pusfofefe`
- `sb2 -t SailfishOS-latest-armv7hl -m sdk-build -R`
    - `qmake`
    - `make`
- `mb2 -t SailfishOS-latest-armv7hl build`
    - or whichever target you require ofcourse
- you now have an RPM in the `RPMS` directory which you can copy to your
  phone and install there

The first time you do the above steps it will all fail because ECL
cannot find the Cloverlover library.  Still, this paragraph is at the
end because by now Quicklisp will have been installed; doing this by
hand for people unfamiliar with Common Lisp is too complicated.  To make
the library available:

- `cd ~/projects`
- `git clone https://git.sr.ht/~aerique/cloverlover`
- `cd ~/quicklisp/local-projects`
- `ln -s ~/projects/cloverlover/cloverlover.asd .`

And build again.

## To Do

### High Priority

- [ ] describe current downloading of messages wrt to threading,
      reasons, all used functions and vars, etc.
- [ ] take a good look a all globals in `pusfofefe.qml` and see if we
      can do better
- [ ] clean up code

### Normal Priority

- [x] use proper primary and hint colors for text, links and input
- [x] add link to https://pushover.net/signup
- [x] use `/home/nemo/.config/pusfofefe/config.lisp` for config
- [x] make refresh button on cover page functional
- [x] proper dialog for logging in
- [x] save messages to a different file than config
    - and now also in the proper location (not in the config but data dir)
- [x] fix display of messages (or just truncate to one line)
- [x] show error page on login failure
- [x] have dropdown list for `*pushover-refresh*`
    - this has only been done for the frontend
- [x] actually get messages every `*pushover-refresh*`
- [x] make sure ASDF doesn't load on app start
- [x] include all Quicklisp packages with app
- [x] expand descriptions for ECL and EQL in About page
- [x] show progress when logging in and retrieving messages
    - this seems a little harder than expected
    - ok, done, the trick was to create the thread in ECL
- [x] also show new messages on cover page
- [ ] describe definition of "new" message
- [ ] check format and timezone of time on MessagePage
    - should be YYYY-MM-DD HH:MM:SS (ISO'ish) and UTC
- [ ] add support for two-factor auth
- [ ] add support searching messages
- [ ] add support showing only messages of a certain type

### Low Priority

- [x] add buttons on MessagePage to go to next and previous message
- [x] add support for landscape orientation
- [ ] figure out why ECL/EQL won't build from `mb2` step below
- [ ] try again to make deploy workflow one command
    - `sb2 -t SailfishOS-latest-armv7hl -m sdk-build -R`
        - `qmake`
        - `make`
    - `mb2 -t SailfishOS-latest-armv7hl build`
    - `scp projects/pusfofefe/RPMS/pusfofefe-0.2-1.armv7hl.rpm nemo@192.168.2.15:`
    - `pkcon -y install-local pusfofefe-0.2-1.armv7hl.rpm`
- [ ] switch from StringList model to an object
    - this will allow us to have more metadata on the message and to
      display it in different ways in the front-end
- [ ] add Pushover [websocket](https://pushover.net/api/client#websocket)
      support

## Attributions

App icon by [Freepik](https://www.flaticon.com/authors/freepik) from [www.flaticon.com](https://www.flaticon.com/).
