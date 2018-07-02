# logody

Just like docker compose but with normal programs.

![](https://github.com/sordina/logody/raw/master/logody_logo.png)

Aggregates process output harmoniously.

Demo: <https://asciinema.org/a/vJMpIfqfVAxHu8iWdRkNYcXW7>

## Disclaimer

This was implemented on a Friday afternoon and there may have been beer involved.

Yes, "logdog" is a much better name. [Unfortunately it's already taken.](https://getlogdog.com/)

Before you send an issue about your program not listing output, check the buffering settings.
Some usual suspects include `grep --line-buffered`, `sed -l`. Remember that GNU and BSD
arguments are often different!

## Features

* Run multiple processes in parallel
* Safely interleve logs
* Indicate process name on log-line
* Indicate handle on log-line (stdout, stderr)
* Indicate exit-codes on log-line after termination
* Aligns process-names and outputs as columns
* Supply shell strings
* Supply executables and lists of arguments
* Configure processes to restart on success
* Configure processes to restart on failure
* Specify subset of configuration processes via arguments
* Convenient arguments-only mode for simple concurrent processes
* YAML configuration file format (feature... or bug?)
* Optionally strips non-printable characters from output
* Kill all processes easily with Ctrl-C

## Installing

This is a Haskell codebase.

[You can install logody with stack.](https://docs.haskellstack.org/en/stable/README/)

## Operation

Two modes of operation:

* Shell Arguments
* Configuration

### Shell Arguments

If you don't supply any config input, then the arguments are interpreted as shell strings.

These will be named "process_N" sequentially and run until completion or failure.

### Usage

    Usage: logody [OPTIONS] [SHELL]* < CONFIG_FILE

        echo -n | logody SHELL*
        or...
        logody [NAME]* < CONFIG_FILE

    Options:

     -h | --help       Print help and usage information.
     -v | --version    Print version information.
     -n | --no-config  Don't read any configuration, just accept shell string arguments.
     -f | --file       Specify config file. '-' for STDIN.

### Configuration

If you supply configuration input, then your process list will be defined in the config file format.

Any arguments provided will be used to filter the process list.

#### Config Example

```
---
osname:
  process: uname
  args:
    - "-a"

echo:
  shell: "echo bar && sleep 1 && exit 1"
  resume:
    - fail

testscript:
  process: ./test/test.bash
  sanitise: False
  resume:
    - succeed
    - fail
```

Output:

```
$ logody < test/processes.yaml
testscript | Starting Process {"name":"testscript","resumption":{"succeed":true,"failure":true},"runner":{"tag":"Program","contents":["./test/test.bash",[]]}}
osname     | Starting Process {"name":"osname","resumption":{"succeed":false,"failure":false},"runner":{"tag":"Program","contents":["uname",["-a"]]}}
echo       | Starting Process {"name":"echo","resumption":{"succeed":false,"failure":true},"runner":{"tag":"Shell","contents":"echo bar && sleep 1 && exit 1"}}
osname     | stdout -> Darwin host.local ...
echo       | stdout -> bar
osname     | Exited Successfully
testscript | stdout -> test.bash 1
testscript | stdout -> test.bash 2
testscript | stdout -> test.bash 3
testscript | stdout -> test.bash 4
echo       | Failure -> Failed with code 1
echo       | Restarting process after failure with exit code 1
echo       | stdout -> bar
testscript | stdout -> test.bash 5
testscript | stdout -> test.bash 6
testscript | stdout -> test.bash 7
echo       | Failure -> Failed with code 1
echo       | Restarting process after failure with exit code 1
echo       | stdout -> bar
testscript | stdout -> test.bash 8
testscript | stdout -> test.bash 9
testscript | stdout -> test.bash 10
echo       | Failure -> Failed with code 1
echo       | Restarting process after failure with exit code 1
echo       | stdout -> bar
testscript | Exited Successfully
testscript | Restarting process after success
testscript | stdout -> test.bash 1
^C
```


## Bugs

* No known bugs!


## TODO

* Support other config formats: JSON, Dhall
* Color Lines
* Implement all of the applicable Docker-Compose features
