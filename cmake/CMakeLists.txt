cmake_minimum_required(VERSION 2.6)
cmake_policy(SET CMP0011 NEW)

project("project")

# only set this if it's the very toplevel project where your module directory is
# located (in this case it's cmake)
# set(ARCB_MODULE_PATH 
#		"${PROJECT_SOURCE_DIR}/cmake/arcb/cmake"
#		CACHE INTERNAL "the cmake module path to arcb")
# set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${ARCB_MODULE_PATH})

include(ARCB)

# should be "toplevel","executable","staticlib",or "sharedlib"
set(buildType "toplevel")

# the source files located in the src/ directory
set(sources "")

#the name of the projects this project depends on
set(subProjects "")

# the internal subprojecs. NOTE: include these subprojects in the order that
# they depend on each other. For example, if proj1 depends on proj2 list proj2
# BEFORE proj1
set(internalSubDirs "")

# DO NOT REMOVE OR MODIFY
BuildProject(
	"${buildType}"
	"${sources}"
	"${subProjects}"
	"${internalSubDirs}"
	)
