""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Vim syntax file
" Language:	glib
" Maintainer:	Jason Thomas <jason@topic.com.au>
" Last Change:	2001 Sep 03

" Now setup the glib specific stuff

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" GLib Fundamentals
" Basic Types
syn keyword	gType gboolean gpointer gconstpointer gchar guchar
syn keyword	gType gint guint gshort gushort glong gulong
syn keyword	gType gint8 guint8 gint16 guint16 gint32 guint32
syn keyword	gDefine G_HAVE_GINT64 G_GINT64_CONSTANT
syn keyword	gType gint64 guint64
syn keyword	gType gfloat gdouble
syn keyword	gType gsize gssize

" Limit of Basic Types
syn keyword	gDefine G_MININT G_MAXINT G_MAXUINT
syn keyword	gDefine G_MINSHORT G_MAXSHORT G_MAXUSHORT
syn keyword	gDefine G_MINLONG G_MAXLONG G_MAXULONG
syn keyword	gDefine G_MININT64 G_MAXINT64 G_MAXUINT64
syn keyword	gDefine G_MINFLOAT G_MAXFLOAT
syn keyword	gDefine G_MINDOUBLE G_MAXDOUBLE

" Standard Macros
syn keyword	gDefine GLIB_MAJOR_VERSION GLIB_MINOR_VERSION GLIB_MICRO_VERSION
syn keyword	gDefine G_OS_WIN32 G_OS_BEOS G_OS_UNIX
syn keyword	gDefine GLIB_CHECK_VERSION
syn keyword	gDefine G_DIR_SEPARATOR G_DIR_SEPARATOR_S
syn keyword	gDefine G_SEARCHPATH_SEPARATOR G_SEARCHPATH_SEPARATOR_S
syn keyword	gDefine TRUE FALSE
syn keyword	gDefine NULL
syn keyword	gDefine MIN MAX
syn keyword	gDefine ABS CLAMP
syn keyword	gDefine G_STRUCT_MEMBER G_STRUCT_MEMBER_P G_STRUCT_OFFSET
syn keyword	gDefine G_MEM_ALIGN
syn keyword	gDefine G_CONST_RETURN

" Type Conversion Macros
syn keyword	gDefine GINT_TO_POINTER GPOINTER_TO_INT
syn keyword	gDefine GUINT_TO_POINTER GPOINTER_TO_UINT
syn keyword	gDefine GSIZE_TO_POINTER GPOINTER_TO_SIZE

" Byte Order Macros
syn keyword	gDefine G_BYTE_ORDER G_LITTLE_ENDIAN G_BIG_ENDIAN G_PDP_ENDIAN
syn keyword	gDefine g_htonl g_htons g_ntohl g_ntohs
syn keyword	gDefine GINT_FROM_BE GINT_FROM_LE GINT_TO_BE GINT_TO_LE
syn keyword	gDefine GUINT_FROM_BE GUINT_FROM_LE GUINT_TO_BE GUINT_TO_LE
syn keyword	gDefine GLONG_FROM_BE GLONG_FROM_LE GLONG_TO_BE GLONG_TO_LE
syn keyword	gDefine GULONG_FROM_BE GULONG_FROM_LE GULONG_TO_BE GULONG_TO_LE
syn keyword	gDefine GINT16_FROM_BE GINT16_FROM_LE GINT16_TO_BE GINT16_TO_LE
syn keyword	gDefine GUINT16_FROM_BE GUINT16_FROM_LE GUINT16_TO_BE GUINT16_TO_LE
syn keyword	gDefine GINT32_FROM_BE GINT32_FROM_LE GINT32_TO_BE GINT32_TO_LE
syn keyword	gDefine GUINT32_FROM_BE GUINT32_FROM_LE GUINT32_TO_BE GUINT32_TO_LE
syn keyword	gDefine GINT64_FROM_BE GINT64_FROM_LE GINT64_TO_BE GINT64_TO_LE
syn keyword	gDefine GUINT64_FROM_BE GUINT64_FROM_LE GUINT64_TO_BE GUINT64_TO_LE
syn keyword	gDefine GUINT16_SWAP_BE_PDP GUINT16_SWAP_LE_BE GUINT16_SWAP_LE_PDP
syn keyword	gDefine GUINT32_SWAP_BE_PDP GUINT32_SWAP_LE_BE GUINT32_SWAP_LE_PDP
syn keyword	gDefine GUINT64_SWAP_LE_BE

" Numerical Definitions
syn keyword	gDefine G_IEEE754_FLOAT_BIAS G_IEEE754_DOUBLE_BIAS
syn keyword	gStructure GFloatIEEE754 GDoubleIEEE754
syn keyword	gDefine G_E G_LN2 G_LN10 G_PI G_PI_2 G_PI_4 G_SQRT2 G_LOG_2_BASE_10

" Miscellaneous Macros
syn keyword	gDefine G_INLINE_FUNC
syn keyword	gDefine G_STMT_START G_STMT_END
syn keyword	gDefine G_BEGIN_DECLS G_END_DECLS 
syn keyword	gDefine G_N_ELEMENTS
syn keyword	gDefine G_VA_COPY
syn keyword	gDefine G_STRINGIFY
syn keyword	gDefine G_GNUC_EXTENSION G_GNUC_CONST G_GNUC_NORETURN G_GNUC_UNUSED
syn keyword	gDefine G_GNUC_PURE G_GNUC_PRINTF G_GNUC_SCANF G_GNUC_FORMAT
syn keyword	gDefine G_GNUC_FUNCTION G_GNUC_PRETTY_FUNCTION G_GNUC_NO_INSTRUMENT
syn keyword	gDefine G_STRLOC
syn keyword	gDefine G_GINT16_FORMAT G_GUINT16_FORMAT G_GINT32_FORMAT G_GUINT32_FORMAT
syn keyword	gDefine G_GINT64_FORMAT G_GUINT64_FORMAT


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" GLib Core Application Support
" The Main Event Loop
syn keyword	gStructure GMainLoop
syn keyword	gDefine g_main_new g_main_destroy g_main_run g_main_quit g_main_is_running
syn keyword	gDefine G_PRIORITY_HIGH G_PRIORITY_DEFAULT
syn keyword	gDefine G_PRIORITY_HIGH_IDLE G_PRIORITY_DEFAULT_IDLE
syn keyword	gDefine G_PRIORITY_LOW
syn keyword	gStructure GMainContext
syn keyword	gDefine g_main_iteration g_main_pending
syn keyword	gDefine g_main_set_poll_func
syn keyword	gStructure GPollFD GSource GSourceFuncs GSourceFunc GSourceCallbackFuncs

" Threads
syn keyword	gDefine G_THREADS_ENABLED G_THREADS_IMPL_POSIX
syn keyword	gDefine G_THREADS_IMPL_SOLARIS G_THREADS_IMPL_NONE
syn keyword	gDefine G_THREAD_ERROR
syn keyword	gEnum GThreadError
syn keyword	gEnumConstant G_THREAD_ERROR_AGAIN
syn keyword	gStructure GThreadFunctions
syn keyword	gEnum GThreadPriority
syn keyword	gEnumConstant G_THREAD_PRIORITY_LOW G_THREAD_PRIORITY_NORMAL
syn keyword	gEnumConstant G_THREAD_PRIORITY_HIGH G_THREAD_PRIORITY_URGENT
syn keyword	gStructure GThread GMutex GStaticMutex
syn keyword	gDefine G_STATIC_MUTEX_INIT
syn keyword	gDefine G_LOCK_DEFINE G_LOCK_DEFINE_STATIC
syn keyword	gDefine G_LOCK_EXTERN G_LOCK
syn keyword	gDefine G_TRYLOCK G_UNLOCK
syn keyword	gStructure GStaticRecMutex
syn keyword	gDefine G_STATIC_REC_MUTEX_INIT
syn keyword	gStructure GStaticRWLock
syn keyword	gDefine G_STATIC_RW_LOCK_INIT
syn keyword	gStructure GCond GStaticPrivate
syn keyword	gDefine G_STATIC_PRIVATE_INIT


" Thread Pools
syn keyword	gStructure GThreadPool

" Asynchronous Queues
syn keyword	gStructure GAsyncQueue

" Dynamic Loading of Modules
syn keyword	gStructure GModule
syn keyword	gEnum GModuleFlags
syn keyword	gEnumConstant G_MODULE_BIND_LAZY G_MODULE_BIND_MASK
syn keyword	gDefine G_MODULE_SUFFIX G_MODULE_EXPORT G_MODULE_IMPORT

" Memory Allocation
syn keyword	gDefine g_new g_new0 g_renew
syn keyword	gDefine g_alloca g_newa g_memmove
syn keyword	gStructure GMemVTable

" IO Channels
syn keyword	gStructure GIOChannel
syn keyword	gEnum GSeekType
syn keyword	gEnumConstant G_SEEK_CUR G_SEEK_SET G_SEEK_END
syn keyword	gEnum GIOStatus
syn keyword	gEnumConstant G_IO_STATUS_ERROR G_IO_STATUS_NORMAL
syn keyword	gEnumConstant G_IO_STATUS_EOF G_IO_STATUS_AGAIN
syn keyword	gEnum GIOChannelError
syn keyword	gEnumConstant G_IO_CHANNEL_ERROR_FBIG G_IO_CHANNEL_ERROR_INVAL
syn keyword	gEnumConstant G_IO_CHANNEL_ERROR_IO G_IO_CHANNEL_ERROR_ISDIR
syn keyword	gEnumConstant G_IO_CHANNEL_ERROR_NOSPC G_IO_CHANNEL_ERROR_NXIO
syn keyword	gEnumConstant G_IO_CHANNEL_ERROR_OVERFLOW G_IO_CHANNEL_ERROR_PIPE
syn keyword	gEnumConstant G_IO_CHANNEL_ERROR_FAILED
syn keyword	gDefine G_IO_CHANNEL_ERROR
syn keyword	gEnum GIOCondition
syn keyword	gEnumConstant G_IO_IN G_IO_OUT G_IO_PRI G_IO_ERR
syn keyword	gEnumConstant G_IO_HUP G_IO_NVAL
syn keyword	gStructure GIOFuncs GIOFlags
syn keyword	gEnum GIOError
syn keyword	gEnumConstant G_IO_ERROR_NONE G_IO_ERROR_AGAIN
syn keyword	gEnumConstant G_IO_ERROR_INVAL G_IO_ERROR_UNKNOWN

" Error Reporting
syn keyword	gStructure GError

" Message Output and Debugging Functions
syn keyword	gDefine g_return_if_fail g_return_val_if_fail
syn keyword	gDefine g_return_if_reached g_return_val_if_reached
syn keyword	gDefine g_assert g_assert_not_reached
syn keyword	gDefine G_BREAKPOINT

" Message Logging
syn keyword	gDefine G_LOG_DOMAIN G_LOG_FATAL_MASK G_LOG_LEVEL_USER_SHIFT
syn keyword	gStructure GLogLevelFlags
syn keyword	gConstant G_LOG_FLAG_RECURSION G_LOG_FLAG_FATAL
syn keyword	gConstant G_LOG_LEVEL_ERROR G_LOG_LEVEL_CRITICAL
syn keyword	gConstant G_LOG_LEVEL_WARNING G_LOG_LEVEL_MESSAGE
syn keyword	gConstant G_LOG_LEVEL_INFO G_LOG_LEVEL_DEBUG
syn keyword	gConstant G_LOG_LEVEL_MASK
syn keyword	gDefine g_message g_warning g_critical g_error


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" GLib Utilities
" String Utility Functions
syn keyword	gDefine G_ASCII_DTOSTR_BUF_SIZE g_strstrip G_STR_DELIMITERS

" Character Set Conversion
syn keyword	gDefine G_CONVERT_ERROR
syn keyword	gEnum GConvertError
syn keyword	gEnumConstant G_CONVERT_ERROR_NO_CONVERSION
syn keyword	gEnumConstant G_CONVERT_ERROR_ILLEGAL_SEQUENCE
syn keyword	gEnumConstant G_CONVERT_ERROR_FAILED
syn keyword	gEnumConstant G_CONVERT_ERROR_PARTIAL_INPUT
syn keyword	gEnumConstant G_CONVERT_ERROR_BAD_URI
syn keyword	gEnumConstant G_CONVERT_ERROR_NOT_ABSOLUTE_PATH

" Unicode Manipulation
syn keyword	gType gunichar gunichar2
syn keyword	gEnum GUnicodeType
syn keyword	gEnumConstant G_UNICODE_CONTROL G_UNICODE_FORMAT
syn keyword	gEnumConstant G_UNICODE_UNASSIGNED G_UNICODE_PRIVATE_USE
syn keyword	gEnumConstant G_UNICODE_SURROGATE G_UNICODE_LOWERCASE_LETTER
syn keyword	gEnumConstant G_UNICODE_MODIFIER_LETTER G_UNICODE_OTHER_LETTER
syn keyword	gEnumConstant G_UNICODE_TITLECASE_LETTER
syn keyword	gEnumConstant G_UNICODE_UPPERCASE_LETTER
syn keyword	gEnumConstant G_UNICODE_COMBINING_MARK G_UNICODE_ENCLOSING_MARK
syn keyword	gEnumConstant G_UNICODE_NON_SPACING_MARK
syn keyword	gEnumConstant G_UNICODE_DECIMAL_NUMBER
syn keyword	gEnumConstant G_UNICODE_LETTER_NUMBER G_UNICODE_OTHER_NUMBER
syn keyword	gEnumConstant G_UNICODE_CONNECT_PUNCTUATION
syn keyword	gEnumConstant G_UNICODE_DASH_PUNCTUATION
syn keyword	gEnumConstant G_UNICODE_CLOSE_PUNCTUATION
syn keyword	gEnumConstant G_UNICODE_FINAL_PUNCTUATION
syn keyword	gEnumConstant G_UNICODE_INITIAL_PUNCTUATION
syn keyword	gEnumConstant G_UNICODE_OTHER_PUNCTUATION
syn keyword	gEnumConstant G_UNICODE_OPEN_PUNCTUATION
syn keyword	gEnumConstant G_UNICODE_CURRENCY_SYMBOL
syn keyword	gEnumConstant G_UNICODE_MODIFIER_SYMBOL G_UNICODE_MATH_SYMBOL
syn keyword	gEnumConstant G_UNICODE_OTHER_SYMBOL G_UNICODE_LINE_SEPARATOR
syn keyword	gEnumConstant G_UNICODE_PARAGRAPH_SEPARATOR
syn keyword	gEnumConstant G_UNICODE_SPACE_SEPARATOR
syn keyword	gEnum GUnicodeBreakType
syn keyword	gEnumConstant G_UNICODE_BREAK_MANDATORY
syn keyword	gEnumConstant G_UNICODE_BREAK_CARRIAGE_RETURN
syn keyword	gEnumConstant G_UNICODE_BREAK_LINE_FEED
syn keyword	gEnumConstant G_UNICODE_BREAK_COMBINING_MARK
syn keyword	gEnumConstant G_UNICODE_BREAK_SURROGATE
syn keyword	gEnumConstant G_UNICODE_BREAK_ZERO_WIDTH_SPACE
syn keyword	gEnumConstant G_UNICODE_BREAK_INSEPARABLE
syn keyword	gEnumConstant G_UNICODE_BREAK_NON_BREAKING_GLUE
syn keyword	gEnumConstant G_UNICODE_BREAK_CONTINGENT
syn keyword	gEnumConstant G_UNICODE_BREAK_SPACE G_UNICODE_BREAK_AFTER
syn keyword	gEnumConstant G_UNICODE_BREAK_BEFORE
syn keyword	gEnumConstant G_UNICODE_BREAK_BEFORE_AND_AFTER
syn keyword	gEnumConstant G_UNICODE_BREAK_HYPHEN G_UNICODE_BREAK_NON_STARTER
syn keyword	gEnumConstant G_UNICODE_BREAK_OPEN_PUNCTUATION
syn keyword	gEnumConstant G_UNICODE_BREAK_CLOSE_PUNCTUATION
syn keyword	gEnumConstant G_UNICODE_BREAK_QUOTATION
syn keyword	gEnumConstant G_UNICODE_BREAK_EXCLAMATION
syn keyword	gEnumConstant G_UNICODE_BREAK_IDEOGRAPHIC
syn keyword	gEnumConstant G_UNICODE_BREAK_NUMERIC
syn keyword	gEnumConstant G_UNICODE_BREAK_INFIX_SEPARATOR
syn keyword	gEnumConstant G_UNICODE_BREAK_SYMBOL G_UNICODE_BREAK_ALPHABETIC
syn keyword	gEnumConstant G_UNICODE_BREAK_PREFIX G_UNICODE_BREAK_POSTFIX
syn keyword	gEnumConstant G_UNICODE_BREAK_COMPLEX_CONTEXT
syn keyword	gEnumConstant G_UNICODE_BREAK_AMBIGUOUS G_UNICODE_BREAK_UNKNOWN
syn keyword	gDefine g_utf8_next_char               
syn keyword	gEnum GNormalizeMode
syn keyword	gEnumConstant G_NORMALIZE_DEFAULT G_NORMALIZE_NFD
syn keyword	gEnumConstant G_NORMALIZE_DEFAULT_COMPOSE G_NORMALIZE_NFC
syn keyword	gEnumConstant G_NORMALIZE_ALL G_NORMALIZE_NFKD
syn keyword	gEnumConstant G_NORMALIZE_ALL_COMPOSE G_NORMALIZE_NFKC

" Date and Time Functions
syn keyword	gDefine G_USEC_PER_SEC
syn keyword	gStructure GTimeVal GDate
syn keyword	gType GTime
syn keyword	gEnum GDateDMY
syn keyword	gEnumConstant G_DATE_DAY G_DATE_MONTH G_DATE_YEAR 
syn keyword	gType GDateDay
syn keyword	gEnum GDateMonth
syn keyword	gEnumConstant G_DATE_BAD_MONTH G_DATE_JANUARY G_DATE_FEBRUARY
syn keyword	gEnumConstant G_DATE_MARCH G_DATE_APRIL G_DATE_MAY G_DATE_JUNE
syn keyword	gEnumConstant G_DATE_JULY G_DATE_AUGUST G_DATE_SEPTEMBER
syn keyword	gEnumConstant G_DATE_OCTOBER G_DATE_NOVEMBER G_DATE_DECEMBER
syn keyword	gType GDateYear
syn keyword	gEnum GDateWeekday
syn keyword	gEnumConstant G_DATE_BAD_WEEKDAY G_DATE_MONDAY G_DATE_TUESDAY
syn keyword	gEnumConstant G_DATE_WEDNESDAY G_DATE_THURSDAY G_DATE_FRIDAY
syn keyword	gEnumConstant G_DATE_SATURDAY G_DATE_SUNDAY
syn keyword	gDefine G_DATE_BAD_DAY G_DATE_BAD_JULIAN G_DATE_BAD_YEAR

" Random Numbers
syn keyword	gStructure GRand
syn keyword	gDefine g_rand_boolean g_random_boolean

" Hook Functions
syn keyword	gStructure GHookList GHook
syn keyword	gDefine g_hook_append   
syn keyword	gEnum GHookFlagMask
syn keyword	gEnumConstant G_HOOK_FLAG_ACTIVE G_HOOK_FLAG_IN_CALL
syn keyword	gEnumConstant G_HOOK_FLAG_MASK
syn keyword	gDefine G_HOOK_FLAGS G_HOOK_FLAG_USER_SHIFT
syn keyword	gDefine G_HOOK G_HOOK_IS_VALID
syn keyword	gDefine G_HOOK_ACTIVE G_HOOK_IN_CALL
syn keyword	gDefine G_HOOK_IS_UNLINKED

" Miscellaneous Utility Functions
syn keyword	gDefine g_dirname
syn keyword	gStructure GDebugKey

" Lexical Scanner
syn keyword	gStructure GScanner
syn keyword	gDefine g_scanner_freeze_symbol_table
syn keyword	gDefine g_scanner_thaw_symbol_table
syn keyword	gEnum GTokenType
syn keyword	gEnumConstant G_TOKEN_EOF
syn keyword	gEnumConstant G_TOKEN_LEFT_PAREN G_TOKEN_RIGHT_PAREN
syn keyword	gEnumConstant G_TOKEN_LEFT_CURLY G_TOKEN_RIGHT_CURLY
syn keyword	gEnumConstant G_TOKEN_LEFT_BRACE G_TOKEN_RIGHT_BRACE
syn keyword	gEnumConstant G_TOKEN_EQUAL_SIGN G_TOKEN_COMMA
syn keyword	gEnumConstant G_TOKEN_NONE G_TOKEN_ERROR
syn keyword	gEnumConstant G_TOKEN_CHAR G_TOKEN_BINARY
syn keyword	gEnumConstant G_TOKEN_OCTAL G_TOKEN_INT
syn keyword	gEnumConstant G_TOKEN_HEX G_TOKEN_FLOAT
syn keyword	gEnumConstant G_TOKEN_STRING G_TOKEN_SYMBOL
syn keyword	gEnumConstant G_TOKEN_IDENTIFIER G_TOKEN_IDENTIFIER_NULL
syn keyword	gEnumConstant G_TOKEN_COMMENT_SINGLE G_TOKEN_COMMENT_MULTI
syn keyword	gEnumConstant G_TOKEN_LAST 
syn keyword	gUnion GTokenValue
syn keyword	gEnum GErrorType
syn keyword	gEnumConstant G_ERR_UNKNOWN G_ERR_UNEXP_EOF
syn keyword	gEnumConstant G_ERR_UNEXP_EOF_IN_STRING
syn keyword	gEnumConstant G_ERR_UNEXP_EOF_IN_COMMENT
syn keyword	gEnumConstant G_ERR_NON_DIGIT_IN_CONST
syn keyword	gEnumConstant G_ERR_DIGIT_RADIX G_ERR_FLOAT_RADIX
syn keyword	gEnumConstant G_ERR_FLOAT_MALFORMED
syn keyword	gDefine G_CSET_a_2_z G_CSET_A_2_Z
syn keyword	gDefine G_CSET_DIGITS G_CSET_LATINC
syn keyword	gDefine G_CSET_LATINS
syn keyword	gDefine g_scanner_add_symbol g_scanner_remove_symbol
syn keyword	gDefine g_scanner_foreach_symbol 

" Automatic String Completion
syn keyword	gStructure GCompletion

" Timers
syn keyword	gStructure GTimer

" Spawning Processes
syn keyword	gEnum GSpawnError
syn keyword	gEnumConstant G_SPAWN_ERROR_FORK G_SPAWN_ERROR_READ
syn keyword	gEnumConstant G_SPAWN_ERROR_CHDIR G_SPAWN_ERROR_ACCES
syn keyword	gEnumConstant G_SPAWN_ERROR_PERM G_SPAWN_ERROR_2BIG
syn keyword	gEnumConstant G_SPAWN_ERROR_NOEXEC G_SPAWN_ERROR_NAMETOOLONG
syn keyword	gEnumConstant G_SPAWN_ERROR_NOENT G_SPAWN_ERROR_NOMEM
syn keyword	gEnumConstant G_SPAWN_ERROR_NOTDIR G_SPAWN_ERROR_LOOP
syn keyword	gEnumConstant G_SPAWN_ERROR_TXTBUSY G_SPAWN_ERROR_IO
syn keyword	gEnumConstant G_SPAWN_ERROR_NFILE G_SPAWN_ERROR_MFILE
syn keyword	gEnumConstant G_SPAWN_ERROR_INVAL G_SPAWN_ERROR_ISDIR
syn keyword	gEnumConstant G_SPAWN_ERROR_LIBBAD G_SPAWN_ERROR_FAILED
syn keyword	gDefine G_SPAWN_ERROR
syn keyword	gEnum GSpawnFlags
syn keyword	gEnumConstant G_SPAWN_LEAVE_DESCRIPTORS_OPEN
syn keyword	gEnumConstant G_SPAWN_DO_NOT_REAP_CHILD
syn keyword	gEnumConstant G_SPAWN_SEARCH_PATH
syn keyword	gEnumConstant G_SPAWN_STDOUT_TO_DEV_NULL
syn keyword	gEnumConstant G_SPAWN_STDERR_TO_DEV_NULL
syn keyword	gEnumConstant G_SPAWN_CHILD_INHERITS_STDIN
syn keyword	gEnumConstant G_SPAWN_FILE_AND_ARGV_ZERO

" File Utilities
syn keyword	gEnum FileError
syn keyword	gEnumConstant G_FILE_ERROR_EXIST G_FILE_ERROR_ISDIR
syn keyword	gEnumConstant G_FILE_ERROR_ACCES G_FILE_ERROR_NAMETOOLONG
syn keyword	gEnumConstant G_FILE_ERROR_NOENT G_FILE_ERROR_NOTDIR
syn keyword	gEnumConstant G_FILE_ERROR_NXIO G_FILE_ERROR_NODEV
syn keyword	gEnumConstant G_FILE_ERROR_ROFS G_FILE_ERROR_TXTBSY
syn keyword	gEnumConstant G_FILE_ERROR_FAULT G_FILE_ERROR_LOOP
syn keyword	gEnumConstant G_FILE_ERROR_NOSPC G_FILE_ERROR_NOMEM
syn keyword	gEnumConstant G_FILE_ERROR_MFILE G_FILE_ERROR_NFILE
syn keyword	gEnumConstant G_FILE_ERROR_BADF G_FILE_ERROR_INVAL
syn keyword	gEnumConstant G_FILE_ERROR_PIPE G_FILE_ERROR_AGAIN
syn keyword	gEnumConstant G_FILE_ERROR_INTR G_FILE_ERROR_IO
syn keyword	gEnumConstant G_FILE_ERROR_PERM G_FILE_ERROR_FAILED
syn keyword	gDefine G_FILE_ERROR
syn keyword	gEnum GFileTest
syn keyword	gEnumConstant G_FILE_TEST_IS_REGULAR G_FILE_TEST_IS_SYMLINK
syn keyword	gEnumConstant G_FILE_TEST_IS_DIR G_FILE_TEST_IS_EXECUTABLE
syn keyword	gEnumConstant G_FILE_TEST_EXISTS
syn keyword	gStructure GDir

" Shell-related Utilities
syn keyword	gEnum GShellError
syn keyword	gEnumConstant G_SHELL_ERROR_BAD_QUOTING
syn keyword	gEnumConstant G_SHELL_ERROR_EMPTY_STRING
syn keyword	gEnumConstant G_SHELL_ERROR_FAILED
syn keyword	gDefine G_SHEL_ERROR

" Glob-style pattern matching
syn keyword	gStructure GPatternSpec

" Simple XML Subset Parser
syn keyword	gEnum GMarkupError
syn keyword	gEnumConstant G_MARKUP_ERROR_BAD_UTF8 G_MARKUP_ERROR_EMPTY
syn keyword	gEnumConstant G_MARKUP_ERROR_PARSE
syn keyword	gEnumConstant G_MARKUP_ERROR_UNKNOWN_ELEMENT
syn keyword	gEnumConstant G_MARKUP_ERROR_UNKNOWN_ATTRIBUTE
syn keyword	gEnumConstant G_MARKUP_ERROR_INVALID_CONTENT
syn keyword	gDefine G_MARKUP_ERROR
syn keyword	gEnum GMarkupParseFlags
syn keyword	gEnumConstant G_MARKUP_DO_NOT_USE_THIS_UNSUPPORTED_FLAG
syn keyword	gStructure GMarkupParseContext GMarkupParser

" Windows Compatibility Functions
syn keyword	gDefine MAXPATHLEN
syn keyword	gType pid_t
syn keyword	gDefine pipe ftruncate G_WIN32_DLLMAIN_FOR_DLL_NAME   


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" GLib Data Types
" Memory Chunks
syn keyword	gStructure GMemChunk

" Doubly-Linked Lists
syn keyword	gStructure GList
syn keyword	gDefine g_list_previous g_list_next

" Singly-Linked Lists
syn keyword	gStructure GSList
syn keyword	gDefine g_slist_next

" Double-ended Queues
syn keyword	gStructure GQueue

" Trash Stacks
syn keyword	gStructure GTrashStack

" Hash Tables
syn keyword	gStructure GHashTable
syn keyword	gDefine g_hash_table_freeze g_hash_table_thaw

" Strings
syn keyword	gStructure GString
syn keyword	gDefine g_string_sprintf g_string_sprintfa

" String Chunks
syn keyword	gStructure GStringChunk

" Arrays
syn keyword	gStructure GArray
syn keyword	gDefine g_array_append_val g_array_prepend_val
syn keyword	gDefine g_array_insert_val g_array_index

" Pointer Arrays
syn keyword	gStructure GPtrArray
syn keyword	gDefine g_ptr_array_index

" Byte Arrays
syn keyword	gStructure GByteArray

" Balanced Binary Trees
syn keyword	gStructure GTree
syn keyword	gEnum GTraverseType
syn keyword	gEnumConstant G_IN_ORDER G_PRE_ORDER
syn keyword	gEnumConstant G_POST_ORDER G_LEVEL_ORDER

" N-ary Trees
syn keyword	gStructure GNode
syn keyword	gDefine g_node_append
syn keyword	gDefine g_node_insert_data
syn keyword	gDefine g_node_insert_data_before
syn keyword	gDefine g_node_append_data
syn keyword	gDefine g_node_prepend_data
syn keyword	gEnum GTraverseFlags;
syn keyword	gEnumConstant G_TRAVERSE_LEAFS G_TRAVERSE_NON_LEAFS
syn keyword	gEnumConstant G_TRAVERSE_ALL G_TRAVERSE_MASK
syn keyword	gDefine g_node_first_child 
syn keyword	gDefine g_node_next_sibling
syn keyword	gDefine g_node_prev_sibling
syn keyword	gDefine G_NODE_IS_LEAF
syn keyword	gDefine G_NODE_IS_ROOT

" Quarks
syn keyword	gType GQuark

" Keyed Data Lists
syn keyword	gStructure GData
syn keyword	gDefine g_datalist_id_set_data g_datalist_id_remove_data
syn keyword	gDefine g_datalist_set_data g_datalist_set_data_full
syn keyword	gDefine g_datalist_get_data g_datalist_remove_data
syn keyword	gDefine g_datalist_remove_no_notify

" Datasets
syn keyword	gDefine g_dataset_id_set_data
syn keyword	gDefine g_dataset_id_remove_data
syn keyword	gDefine g_dataset_set_data g_dataset_set_data_full
syn keyword	gDefine g_dataset_get_data g_dataset_remove_data
syn keyword	gDefine g_dataset_remove_no_notify

" Relations and Tuples
syn keyword	gStructure GRelation GTuples

" Caches
syn keyword	gStructure GCache

" Memory Allocators
syn keyword	gStructure GAllocator

syn keyword gType GFunc AQFunc lua_State luaL_reg luaL_Buffer LuaState LuaLReg

syn keyword gType GOptionContext GSequence GSequenceIter
	


""" GNet Stuff
syn keyword	gType GTcpSocket GTcpSocketConnectAsyncID GTcpSocketConnectAsyncStatus
syn keyword gType GTcpSocketNewAsyncID GTcpSocketNewAsyncStatus
syn keyword gType GInetAddr GConn GConnEvent GKeyFile GRegex
""" CAML Stuff

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_glib_syn_inits")
	if version < 508
		let did_glib_syn_inits = 1
		command -nargs=+ HiLink hi link <args>
	else
		command -nargs=+ HiLink hi def link <args>
	endif

	HiLink gType			Type
	HiLink gStructure		Structure
	HiLink gEnum			Structure
	HiLink gUnion			Structure
	HiLink gConstant		Constant
	HiLink gEnumConstant		Constant
	HiLink gDefine			Macro
	HiLink mlType Type
	HiLink mlMacro Macro

	
	delcommand HiLink
endif

let b:current_syntax = "glib"
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
