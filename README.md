# Pusfofefe

A Pushover client written in Common Lisp.

Thanks to [Renaud Casenave-Péré](https://openrepos.net/user/856/programs)
for packaging both ECL and EQL5.

The canonical home page of this project is https://git.sr.ht/~aerique/pusfofefe

(This project is also pushed to GitLab and GitHub but those sites are
not monitored for support.)

## Usage

1. Go to the Settings page,
1. Enter your Pushover credentials and press the Login button.
1. Go back to the Main page.

Any queued messages should have been retrieved by now.

As long as the app is in the foreground or minimized it will
periodically (configurable in Settings) retrieve new messages.

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

## To Do

- [ ] add instructions on adding Cloverlover to Quicklisp
- [X] use proper primary and hint colors for text, links and input
- [X] add link to https://pushover.net/signup
- [X] use `/home/nemo/.config/pusfofefe/config.lisp` for config
- [X] make refresh button on cover page functional
- [ ] fix display of messages (or just truncate to one line)
- [ ] save messages to a different file than config
- [ ] proper dialog for logging in
- [ ] expand descriptions for ECL and EQL in About page
- [ ] switch from StringList model to an object
    - this will allow us to have more metadata on the message and to
      display it in different ways in the front-end
    - will also allow us easily manipulate the list of messages (f.e. to
      delete individual messages)
- [ ] add support for two-factor auth
- [ ] add support for entering session secrets and device UUIDs

### Maybe

- [ ] add support for landscape orientation
- [ ] add Pushover [websocket](https://pushover.net/api/client#websocket)
      support

## Attributions

App icon by [Freepik](https://www.flaticon.com/authors/freepik) from [www.flaticon.com](https://www.flaticon.com/).
