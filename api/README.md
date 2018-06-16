# Machine Learning Scala Toolbox

## Dependencies
Spark
SparklingWater
Play Framewors

## Status


## Project structure
 
```
├─ app/           - [Play] web MVC
├─ conf/          - [Play] configurations
├─ docs/          - documentation for algorithms etc...
├─ public/        - [Play] assets
├─ src/           - Source code
│  ├─ main/       - Main implementation code 
│  │  ├─ scala/
│  ├─ test/       - Test code
│  │  ├─ scala/
├─ build.gradle   - Build file for this project
```



## Project building
For building, please, use provided `gradlew` command:

```
./gradlew build
```

### Run
For running an application:

```
./gradlew run
```

## Running tests

To run tests, please, run:

```
./gradlew test
```



# Checking code style

To check codestyle:

```
./gradlew scalaStyle
```

## Creating and Running  Application

Create application fat jar  which can be directly run:

```
./gradlew shadowJar
```

The command creates jar file `build/libs/toolbox.jar` containing all necessary classes to run application.
