####
ARCB
####

A Recursive CMake Build System
==============================

CMake pretty much takes care of itself when it comes to developing projects.
However, we don't have time to manually handcrank repetative project information
like include dependencies and libraries for large scale projects.  Also, we need
a system to keep up with the complex interactions of project relations. This is
where the ProjectBuild module comes in. ARCB (pronounced "ark-bee") is a
CMakeBuild system that attempts to simplify the world of c++ development by
allowing a recusive structure.

ARCB aims to be simple to use. It allows users to keep their CMake scripts clean
by generating the project properties and exporting them. This allows a developer
to think of their project, not as depending on just a library, but depending on
another project.

Quick Start
===========

1. Clone the ARCB git repository into your module directory (like
   :code:`cmake/`). Be sure to add the path git repo path of ARCB to your
   .gitignore!

2. Copy :code:`CMakeLists.txt` in :code:`cmake/` (the one inside the ARCB repo)
   to your project's top level directory.

3. Edit the :code:`CMakeLists.txt` to set the project information. Comments are
   in there to help.

4. Create the build directory such as :code:`build`.

5. Run cmake inside the build directory!

Projects
========

A project is simply the code for a given idea (I don't think this needs
to be explained...see a dictionary if you don't understand the concept
of a project). A project is reified by a directory with the following
structure:

  ::

    project-name
    |-- CMakeLists.txt
    |-- include/
    |   `-- project-name/
    |-- src
    |-- subproj1
    `-- ... 

CMakeLists.txt
  This is the CMake interface for the project. Here is where you define
  the properties of the project (source files,subprojects,etc.).

src/
  This is where your source files go. 

include/project-name
  This is where your header files go.

subproj1/
  This is a subproject with the same directory structure as above.
  Subprojects like this should ONLY depend on the projects that are
  inside of :code:`project-name`. 
  
**WARNING:** the term "subproject" can be rather confusing. I'm working on a
better naming system. When you see the term "subproject" think "Project that is
depended on" and when you see "internal sub directory (internalSubDirs)" think
"project contained with".

CMakeLists.txt
==============

See one of the CMakeLists files in this project for an example. Here are
the main parts:

:code:`# set(ARCB_MODULE_PATH ...`
  This is a variable that points to the cmake module path of ARCB. It's used by
  the ARCB cmake script to create new projects (see `Automatically Creating New
  Projects`_). *NOTE*: this variable will not be needed in the future as a
  FindARCB.cmake file will be written in which ARCB will be installed seperately
  on a system. Since ARCB is still in development and different versions are
  carried around with other projects it is a quick hack to for ARCB to quickly
  find where its files are located.

:code:`set(buildType "<build-type>")`
  The  type of build this project is. Here are the following options for
  :code:`<build-type>`:

    toplevel
      This project is a toplevel project. In other words it only
      includes the subprojects and sets up the _LIBRARIES and
      _INCLUDE_DIRS variables to contain all of the _LIBRARIES and
      _INCLUDE_DIRS of the subprojects.

    executable
      This project builds to be an executable.

    staticlib
      This project builds to be a statically linked library.

    sharedlib
      This project builds to be a shared library.

    headerlib
      This project doesn't build anything; it just exports the
      _LIBRARIES and _INCLUDE_DIRS variables to allow other projects to
      include its header files. This is for libraries that are header
      only.

:code:`set(sources "...")`
  A cmake list of source files located in :code:`src/`

:code:`set(subProjects "...")`
  A cmake list of projects that the current project depends on.

:code:`set(internalSubDirs "...")`
  A cmake list of the subprojects located in this project (and are
  **ONLY** dependent on each other). You can add names of projects that don't
  exist which will get created Automatically (see `Automatically Creating New
  Projects`_)

Project Properties
==================

The properties for a project are defined by the CMakeLists.txt file in
each project directory. These properties should be in the form
:code:`${PROJECT_NAME}_<PROPERTY>` (in cmake syntax), where
:code:`PROJECT_NAME` is the name of the project.

The following are properties for a project:

:code:`LIBRARIES`
  The libraries this project requires to build with.

:code:`INCLUDE_DIRS`
  The directories to look in for header files. If the project build-type
  is a library (staticlib,sharedlib,or headerlib) then that project's
  name is also included in its export of this property. This is to allow
  for other projects to include the library project along with its
  dependencies.

External Libraries
==================

External libries such as opencv should be included into the build system as
projects. However, using the directory structure above is not possible to do
this. Use the :code:`find_package(...)` CMake command to include the library in
the projects CMakeLists file. Then in the CMakeLists file for the project add
the library as a subproject. *NOTE*: it is assumed that by calling
:code:`find_package(...)` that the :code:`<LIB>_LIBRARIES` and
:code:`<LIB>_INCLUDE_DIRS` are created for the given library.

For example, lets say we want to include the OpenCV library. Then in our
projects CMakeLists.txt file we will have: 

  ::

    find_package(OpenCV ...)

      ...

    set(subProjects "OpenCV" ...)

Automatically Creating New Projects
===================================

Copying directories isn't hard, it just can be tedious. You can easily add new
projects by:

1. Add the new project name to the :code:`internalSubDirs` list of the
   containing projects CMakeLists.txt file.

2. Run cmake. A new project directory will be created along with its
   CMakeLists.txt file, include directory, and source directory.

3. Edit the new projects cmake file - at least set the project name

4. Rerun cmake to add the project to be built.
