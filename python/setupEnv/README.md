# setupEnv
setupEnv - crossplatform tool for setting envinronment.

## How to run
```
python setupEnv
```

## Add new instance for setup
To adding new instance for setup just add name of it to the __deps.txt__ file.
```
tools:
    myTool0
    myTool1

libs:
    myLib0
    myLib1
```

Tools should be specified after __tools:__ special word and libs should be specified after __libs:__. __tools:__ and __libs:__ first of all mean directories where installation scripts are.

Symbol ":" means the next line is a start of the dependencies of previos one. The line followed after ":" should have one more tab for indentation then previous one. End of dependencies is specified by a new line with minus one tab. List of tools or libs should be ended with empty string or end of file. Tools and libs can be written in every order.

### Example: myTool0 depends on myTool1
```
tools:
    myTool0:
        myTool1
    myTool1
```

### Example: adding dependence myTool1 on myLib0
```
tools:
    myTool0:
        myTool1
    myTool1:
        myLib0

libs:
    myLib0
    myLib1
```

## Add new installation scripts for setup
setupEnv contains some scripts for crossplatform installation different tools and libs, whey can be found in corresponding directories.

If you want to add new one just create new directory in tools/ or libs/ with the name of instance, put __install.py__ script and add you instance to the __deps.txt__ file.

## setupEnv Arguments:
|||
|---------------|--------------------------------------------------------------------------------------|
|-f, --deps-file| Path to file contains high-level setup instances and dependencies. Default: deps.txt |
|||

# !TODO:
*   Add parameters for setup instances: version, install directory.
*   Add deps.txt files for every instance. It's a basic list of target dependencies, but you still can add new ones to root deps.txt.

