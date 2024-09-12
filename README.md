# CLI Pomodoro Clock

Cli-based pomodoro clock with live events. Built in **Haskell** using unix sockets.

## Features

- [x] Configurable times
- [x] Status formatting
- [x] Live hooks:
  - on-work-start
  - on-break-start
  - on-pomodoro-end
- [ ] A config file for configuring the default session values
- [ ] Presistent task history
- [ ] Rofi plugin for easy access

## Installation

...

## Usage

### Strating a pomodoro session

The simplest way to start a pomodoro session is to run:

```
$ pomodoro start
```

To add a task name simply pass it's name as a string:

```
$ pomodoro start "Some task name"
```

There are many session configuration options to use with the `start` command:
| option | short | description | default |
| --- | --- | --- | --- |
| `--work-time` | `-w` | sets the work time in minutes | 25.0 |
| `--short-break-time` | `-s` | sets the time of a short break in minutes | 5.0 |
| `--long-break-time` | `-l` | sets the time of a long break in minutes | 15.0 |
| `--long-break-freq` | `-f` | sets the interval in which a long break will occur | 4 |
| `--cycles` | `-c` | number of cycles (work+break) needed to complete the task | 1 |

_Note_: The clock finishes automatically upon completing the last work, **without going into the break state**. <br />
That means when for example starting a session with 2 cycles, the only break will be the one in between the two work states.

### Checking session status

To check the status of the current session run: <br />

```
$ pomodoro status
Some task 12:32 (Work), 1/2
```

_Note_: if there is no session running, nothing will be returned.

You can also format the status using `--format` or `-f` option:

```
$ pomodoro status -f "🍅 {time}, {cycle}/{goal}"
🍅 12:32, 1/2
```

Available variables to use with the `status` command:
| variable | description |
| --- | --- |
| `{title}` | The title of the task |
| `{time}` | Time (m:s) remaining in the current state |
| `{state}` | Current state |
| `{cycle}` | Current cycle |
| `{goal}` | Cycle goal |

### Other commands

Although it is not recommended, you can use the `toggle` command to pause and resume the clock:

```
$ pomodoro toggle
```

To stop the session completely, use the `stop` command:

```
$ pomodoro stop
```

## Hooks

Hooks are events that trigger when certain conditions are met.
Currently, 3 different hooks are supported.
For each of them, you can create a `.sh` script in the `~/pomodoro` folder, to be executed when the hook is triggered.

Supported hooks:
| hook name | conditon to trigger |
| --- | --- |
| `on-work-start.sh` | clock chages state from break to work |
| `on-break-start.sh` | clock changes state from work to break |
| `on-pomodoro-end.sh` | session end (all cycles completed) |

You can use hooks to send desktop notifications, play sounds, etc.:

```sh
# on-work-start.sh

# send a desktop notification
notify-send "Pomodoro Clock" "Time to get back to work!"

# play a sound
paplay work-start-sound.wav
```
