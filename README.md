# logody

Just like docker compose but with normal programs.

![](https://github.com/sordina/logody/raw/master/logody_logo.png)

Aggregates logs nicely.

Demo: <https://asciinema.org/a/vJMpIfqfVAxHu8iWdRkNYcXW7>

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
