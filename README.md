# CLI Pomodoro Clock

Cli-based pomodoro clock with live events. Built in **Haskell** using unix sockets.

## Features

- [x] Running in the background without blocking the terminal
- [x] Configurable times
- [x] Configurable status message
- [x] Live events for custom notifications, sounds, etc.
- [x] A config file for configuring the default session values
- [ ] Rofi plugin for interacting with the clock
- [ ] Persistent task history

## Installation

### Nix

The simplest way to try pomodoro-clock-cli is to use nix. <br />
Assuming you have nix installed on your system and nix-commands enabled, you can start the clock by running:

```
nix run github:geraldserafin/pomodoro-clock-cli -- start
```

### NixOS

#### With flakes

You can add pomodoro-clock-cli as an input to your `flake.nix` file like so

```
{
    ...

    inputs = {
        ...
        pomodoro-clock-cli.url = "github:geraldserafin/pomodoro-clock-cli";
        pomodoro-clock-cli.inputs.nixpkgs.follows = "nixpkgs";
    }

    ...
}
```

The package is outputed to `pomodoro-clock-cli.packages.<your-system>.default`

## Usage

### Starting a pomodoro session

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

To check the status of the current session run:

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

Although it's inconsistent with the pomodoro technique, you can use the `toggle` command to pause and resume the clock:

```
$ pomodoro toggle
```

To stop the session completely, use the `stop` command:

```
$ pomodoro stop
```

### Running multiple clocks

All of the commands accept a `--socket` argument that lets you specify a path to the socket, enabling you to run multiple clocks at the same time.

## Hooks

Hooks are events that trigger when certain conditions are met.
Currently, 3 different hooks are supported.

Supported hooks:
| hook name | condition to trigger |
| --- | --- |
| `on-work-start` | clock changes state from break to work |
| `on-break-start` | clock changes state from work to break |
| `on-pomodoro-end` | session ends (all cycles are completed) |

You can set a script to run when a hook is triggered by providing a path to the script in a file located in `~/.config/pomodoro/config.yaml`.

```yaml
# ~/.config/pomodoro/config.yaml

hooksSettings:
  onBreakStart: "/path/to/on-break-start.sh"
  onWorkStart: "/path/to/on-work-start.sh"
  onPomodoroEnd: "/path/to/on-pomodoro-end.sh"
```

## Configuration File

The config file also enables you to also override the default settings.

```yaml
# ~/.config/pomodoro/config.yaml

clockSettings:
  title: "Focusing"
  workTime: 25
  shortBreakTime: 5
  longBreakTime: 15
  cycles: 2
  longBreakFrequency: 4

statusSettings:
  format: "{title} {time} ({state}), {cycle}/{goal}"
  workText: "Work"
  shortBreakText: "Short Break"
  longBreakText: "Long Break"

socketPath: "/tmp/pomodoro.sock"
```
