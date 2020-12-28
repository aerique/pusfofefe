# Pusfofefe

A Pushover client written in Common Lisp.

Thanks to [Renaud Casenave-Péré](https://openrepos.net/user/856/programs)
for packaging both ECL and EQL5.

The canonical home page of this project is https://git.sr.ht/~aerique/pusfofefe

(This project is also pushed to GitLab and GitHub but those sites are
not monitored for support.)

## Dependencies

- [SailfishOS Builds in Docker](https://git.sr.ht/~aerique/sfosbid)
- [Cloverlover](https://git.sr.ht/~aerique/cloverlover)

(See below in "Build".)

## Usage

**TBD**

### Troubleshooting

If your device has already been registered the login will fail.  Delete
the device on the Pushover website.

I realize this workflow is awkward.

## Build

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

- [x] use proper primary and hint colors for text, links and input
- [x] add link to https://pushover.net/signup
- [x] use `/home/nemo/.config/pusfofefe/config.lisp` for config
- [x] make refresh button on cover page functional
- [x] proper dialog for logging in
- [x] save messages to a different file than config
- [ ] show progress when logging in
- [ ] show error page on login failure
- [ ] fix display of messages (or just truncate to one line)
- [ ] have dropdown list for `*pushover-refresh*`
- [ ] expand descriptions for ECL and EQL in About page
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
    - will also allow us easily manipulate the list of messages (f.e. to
      delete individual messages)
- [ ] add support for two-factor auth
- [ ] add support searching messages
- [ ] add support showing only messages of a certain type

### Maybe

- [ ] add buttons on MessagePage to go to next and previous message
- [ ] add support for entering session secrets and device UUIDs
- [ ] add support for landscape orientation
- [ ] add Pushover [websocket](https://pushover.net/api/client#websocket)
      support

## Attributions

App icon by [Freepik](https://www.flaticon.com/authors/freepik) from [www.flaticon.com](https://www.flaticon.com/).
