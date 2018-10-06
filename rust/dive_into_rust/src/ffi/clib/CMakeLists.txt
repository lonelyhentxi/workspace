project(RS_CLIB)

set(CCALLER ccaller.c)
set(CLIB ${PROJECT_SOURCE_DIR}/clib.lib)
add_executable(CCALLER ${PROJECT_SOURCE_DIR}/ccaller.c)
target_link_libraries(CCALLER ${CLIB} ws2_32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib UserEnv.Lib)