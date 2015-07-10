cmake_minimum_required(VERSION 2.6)
cmake_policy(SET CMP0011 NEW)

############################# Uttility Functions #############################

#adds an item to the list and removes the duplicates
function(listUpdate
		# the list to update
		lst
		# the item to append to the list
		item
		)

	list(APPEND ${lst} ${item})
	if(${lst})
		list(REMOVE_DUPLICATES ${lst})
	endif(${lst})
	set(${lst} ${${lst}} PARENT_SCOPE)
endfunction(listUpdate)

########################### End Uttility Functions ###########################

################################ BuildProject ################################

function(BuildProject
		# the build type
		#toplevel - this project simply includes sub directories
		#executable - this project produces an executable (.exe,etc.)
		#staticlib - this project builds as a static library (.a)
		#sharedlib - this project builds as a shared library (.so,.dll,etc.)
		buildType

		#list of source files to build (this is a list of files relative to the src
		#dir. For example, if your source files are located (as they should be) in
		#src/ as src/{a.cpp,b.cpp,...} then sources would be: set(sources "a.cpp
		#b.cpp")
		sources

		# the projects this project depends on. Note that circular dependencies are
		# a mess?
		subProjects 

		#subprojects that are contained within this one (as their own
		#subdirectories) these projects are assumed to be dependent on this project
		#and this project only
		internalSubDirs
		)

	#append the source dir to the list of sources
	set(sourcesTemp "")
	foreach(f ${sources})
		listUpdate(sourcesTemp "${CMAKE_CURRENT_SOURCE_DIR}/src/${f}")
	endforeach(f)
	set(sources "${sourcesTemp}")

	# add the executable/library to the project
	if(executable STREQUAL ${buildType})	
		add_executable("${PROJECT_NAME}" ${sources})
	elseif(staticlib STREQUAL ${buildType})
		add_library("${PROJECT_NAME}" STATIC ${sources})
	elseif(sharedlib STREQUAL ${buildType})
		add_library("${PROJECT_NAME}" SHARED ${sources})
	elseif(headerlib STREQUAL ${buildType})
		# don't do anything
	elseif(toplevel STREQUAL ${buildType})
		# don't do anything
	else(executable STREQUAL ${buildType})
		message(FATAL_ERROR 
			"Uknown build type: '${buildType}' for project: '${PROJECT_NAME}'")
	endif(executable STREQUAL ${buildType})

	#init the project information
	set(${PROJECT_NAME}_libraries "")
	set(${PROJECT_NAME}_includeDirs "")

	# add the subProject information to this project
	foreach(p ${subProjects})
		listUpdate(${PROJECT_NAME}_includeDirs "${${p}_INCLUDE_DIRS}")
		listUpdate(${PROJECT_NAME}_libraries "${${p}_LIBRARIES}")
	endforeach(p)

	# create the build's include dir list
	listUpdate(${PROJECT_NAME}_includeDirs ${CMAKE_CURRENT_SOURCE_DIR}/include)
	include_directories(${${PROJECT_NAME}_includeDirs})

	# add the libraries this build needs to link to (only if we're actually
	# building something)
	set(x "executable;staticlib;sharedlib")
	list(FIND x ${buildType} isBuildable)
	if(isBuildable GREATER -1)
		target_link_libraries(${PROJECT_NAME} ${${PROJECT_NAME}_libraries})
	endif(isBuildable GREATER -1)

	#Export variables
	set(${PROJECT_NAME}_INCLUDE_DIRS
		${${PROJECT_NAME}_includeDirs}
		CACHE INTERNAL "the include dirs"
		)

	# add the current project to its own library include list if it's a library
	# this is so other projects that depend on this one will automatically include
	# it in their libraries list without needing to explicitely state it
	if(staticlib STREQUAL ${buildType} OR sharedlib STREQUAL ${buildType})
		listUpdate(${PROJECT_NAME}_libraries ${PROJECT_NAME})
	endif(staticlib STREQUAL ${buildType} OR sharedlib STREQUAL ${buildType})

	set(${PROJECT_NAME}_LIBRARIES
		${${PROJECT_NAME}_libraries}
		CACHE INTERNAL "the libraries for this project"
		)

	#add the subprojects to this project
  #TDOO move this up and add the subdirs project info as part of this project
	foreach(p ${internalSubDirs})
		add_subdirectory(${p})
	endforeach(p)

endfunction(BuildProject)

############################## End BuildProject ##############################
