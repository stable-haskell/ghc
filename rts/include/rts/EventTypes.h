#pragma once

// When adding a new event type used by GHC you should also update
// NUM_GHC_EVENT_TAGS in include/rts/EventLogFormat.h.

EventType eventTypes[] = {
    [EVENT_CREATE_THREAD] = {
      .etNum = 0,
      .size = sizeof(EventThreadID),
      .desc = "Create thread"
    },
    [EVENT_RUN_THREAD] = {
      .etNum = 1,
      .size = sizeof(EventThreadID),
      .desc = "Run thread"
    },
    [EVENT_STOP_THREAD] = {
      .etNum = 2,
      .size = sizeof(EventThreadID) + sizeof(StgWord16) + sizeof(EventThreadID),
      .desc = "Stop thread"
    },
    [EVENT_THREAD_RUNNABLE] = {
      .etNum = 3,
      .size = sizeof(EventThreadID),
      .desc = "Thread runnable"
    },
    [EVENT_MIGRATE_THREAD] = {
      .etNum = 4,
      .size = sizeof(EventThreadID) + sizeof(EventCapNo),
      .desc = "Migrate thread"
    },
    [EVENT_THREAD_WAKEUP] = {
      .etNum = 8,
      .size = sizeof(EventThreadID) + sizeof(EventCapNo),
      .desc = "Wakeup thread"
    },
    [EVENT_GC_START] = {
      .etNum = 9,
      .size = 0,
      .desc = "Starting GC"
    },
    [EVENT_GC_END] = {
      .etNum = 10,
      .size = 0,
      .desc = "Finished GC"
    },
    [EVENT_REQUEST_SEQ_GC] = {
      .etNum = 11,
      .size = 0,
      .desc = "Request sequential GC"
    },
    [EVENT_REQUEST_PAR_GC] = {
      .etNum = 12,
      .size = 0,
      .desc = "Request parallel GC"
    },
    [EVENT_CREATE_SPARK_THREAD] = {
      .etNum = 15,
      .size = sizeof(EventThreadID),
      .desc = "Create spark thread"
    },
    [EVENT_LOG_MSG] = {
      .etNum = 16,
      .size = 0xffff,
      .desc = "Log message"
    },
    [EVENT_BLOCK_MARKER] = {
      .etNum = 18,
      .size = sizeof(StgWord32) + sizeof(EventTimestamp) + sizeof(EventCapNo),
      .desc = "Block marker"
    },
    [EVENT_USER_MSG] = {
      .etNum = 19,
      .size = 0xffff,
      .desc = "User message"
    },
    [EVENT_GC_IDLE] = {
      .etNum = 20,
      .size = 0,
      .desc = "GC idle"
    },
    [EVENT_GC_WORK] = {
      .etNum = 21,
      .size = 0,
      .desc = "GC working"
    },
    [EVENT_GC_DONE] = {
      .etNum = 22,
      .size = 0,
      .desc = "GC done"
    },
    [EVENT_CAPSET_CREATE] = {
      .etNum = 25,
      .size = sizeof(EventCapsetID) + sizeof(EventCapsetType),
      .desc = "Create capability set"
    },
    [EVENT_CAPSET_DELETE] = {
      .etNum = 26,
      .size = sizeof(EventCapsetID),
      .desc = "Delete capability set"
    },
    [EVENT_CAPSET_ASSIGN_CAP] = {
      .etNum = 27,
      .size = sizeof(EventCapsetID) + sizeof(EventCapNo),
      .desc = "Add capability to capability set"
    },
    [EVENT_CAPSET_REMOVE_CAP] = {
      .etNum = 28,
      .size = sizeof(EventCapsetID) + sizeof(EventCapNo),
      .desc = "Remove capability from capability set"
    },
    [EVENT_RTS_IDENTIFIER] = {
      .etNum = 29,
      .size = 0xffff,
      .desc = "RTS name and version"
    },
    [EVENT_PROGRAM_ARGS] = {
      .etNum = 30,
      .size = 0xffff,
      .desc = "Program arguments"
    },
    [EVENT_PROGRAM_ENV] = {
      .etNum = 31,
      .size = 0xffff,
      .desc = "Program environment variables"
    },
    [EVENT_OSPROCESS_PID] = {
      .etNum = 32,
      .size = sizeof(EventCapsetID) + sizeof(StgWord32),
      .desc = "Process ID"
    },
    [EVENT_OSPROCESS_PPID] = {
      .etNum = 33,
      .size = sizeof(EventCapsetID) + sizeof(StgWord32),
      .desc = "Parent process ID"
    },
    [EVENT_SPARK_COUNTERS] = {
      .etNum = 34,
      .size = sizeof(StgWord64) + sizeof(StgWord64) + sizeof(StgWord64) + sizeof(StgWord64) + sizeof(StgWord64) + sizeof(StgWord64) + sizeof(StgWord64),
      .desc = "Spark counters"
    },
    [EVENT_SPARK_CREATE] = {
      .etNum = 35,
      .size = 0,
      .desc = "Spark create"
    },
    [EVENT_SPARK_DUD] = {
      .etNum = 36,
      .size = 0,
      .desc = "Spark dud"
    },
    [EVENT_SPARK_OVERFLOW] = {
      .etNum = 37,
      .size = 0,
      .desc = "Spark overflow"
    },
    [EVENT_SPARK_RUN] = {
      .etNum = 38,
      .size = 0,
      .desc = "Spark run"
    },
    [EVENT_SPARK_STEAL] = {
      .etNum = 39,
      .size = sizeof(EventCapNo),
      .desc = "Spark steal"
    },
    [EVENT_SPARK_FIZZLE] = {
      .etNum = 40,
      .size = 0,
      .desc = "Spark fizzle"
    },
    [EVENT_SPARK_GC] = {
      .etNum = 41,
      .size = 0,
      .desc = "Spark GC"
    },
    [EVENT_INTERN_STRING] = {
      .etNum = 42,
      .size = 0xffff,
      .desc = "Intern string"
    },
    [EVENT_WALL_CLOCK_TIME] = {
      .etNum = 43,
      .size = sizeof(EventCapsetID) + sizeof(StgWord64) + sizeof(StgWord32),
      .desc = "Wall clock time"
    },
    [EVENT_THREAD_LABEL] = {
      .etNum = 44,
      .size = 0xffff,
      .desc = "Thread label"
    },
    [EVENT_CAP_CREATE] = {
      .etNum = 45,
      .size = sizeof(EventCapNo),
      .desc = "Create capability"
    },
    [EVENT_CAP_DELETE] = {
      .etNum = 46,
      .size = sizeof(EventCapNo),
      .desc = "Delete capability"
    },
    [EVENT_CAP_DISABLE] = {
      .etNum = 47,
      .size = sizeof(EventCapNo),
      .desc = "Disable capability"
    },
    [EVENT_CAP_ENABLE] = {
      .etNum = 48,
      .size = sizeof(EventCapNo),
      .desc = "Enable capability"
    },
    [EVENT_HEAP_ALLOCATED] = {
      .etNum = 49,
      .size = sizeof(EventCapsetID) + sizeof(StgWord64),
      .desc = "Total heap memory ever allocated"
    },
    [EVENT_HEAP_SIZE] = {
      .etNum = 50,
      .size = sizeof(EventCapsetID) + sizeof(StgWord64),
      .desc = "Current heap size (number of allocated mblocks)"
    },
    [EVENT_HEAP_LIVE] = {
      .etNum = 51,
      .size = sizeof(EventCapsetID) + sizeof(StgWord64),
      .desc = "Current heap live data"
    },
    [EVENT_HEAP_INFO_GHC] = {
      .etNum = 52,
      .size = sizeof(EventCapsetID) + sizeof(StgWord16) + sizeof(StgWord64) + sizeof(StgWord64) + sizeof(StgWord64) + sizeof(StgWord64),
      .desc = "Heap static parameters"
    },
    [EVENT_GC_STATS_GHC] = {
      .etNum = 53,
      .size = sizeof(EventCapsetID) + sizeof(StgWord16) + sizeof(StgWord64) + sizeof(StgWord64) + sizeof(StgWord64) + sizeof(StgWord32) + sizeof(StgWord64) + sizeof(StgWord64) + sizeof(StgWord64),
      .desc = "GC statistics"
    },
    [EVENT_GC_GLOBAL_SYNC] = {
      .etNum = 54,
      .size = 0,
      .desc = "Synchronise stop-the-world GC"
    },
    [EVENT_TASK_CREATE] = {
      .etNum = 55,
      .size = sizeof(EventTaskId) + sizeof(EventCapNo) + sizeof(EventKernelThreadId),
      .desc = "Task create"
    },
    [EVENT_TASK_MIGRATE] = {
      .etNum = 56,
      .size = sizeof(EventTaskId) + sizeof(EventCapNo) + sizeof(EventCapNo),
      .desc = "Task migrate"
    },
    [EVENT_TASK_DELETE] = {
      .etNum = 57,
      .size = sizeof(EventTaskId),
      .desc = "Task delete"
    },
    [EVENT_USER_MARKER] = {
      .etNum = 58,
      .size = 0xffff,
      .desc = "User marker"
    },
    [EVENT_HACK_BUG_T9003] = {
      .etNum = 59,
      .size = 0,
      .desc = "Empty event for bug #9003"
    },
    [EVENT_MEM_RETURN] = {
      .etNum = 90,
      .size = sizeof(EventCapsetID) + sizeof(StgWord32) + sizeof(StgWord32) + sizeof(StgWord32),
      .desc = "The RTS attempted to return heap memory to the OS"
    },
    [EVENT_BLOCKS_SIZE] = {
      .etNum = 91,
      .size = sizeof(EventCapsetID) + sizeof(StgWord64),
      .desc = "Report the size of the heap in blocks"
    },
    [EVENT_HEAP_PROF_BEGIN] = {
      .etNum = 160,
      .size = 0xffff,
      .desc = "Start of heap profile"
    },
    [EVENT_HEAP_PROF_COST_CENTRE] = {
      .etNum = 161,
      .size = 0xffff,
      .desc = "Cost-centre definition"
    },
    [EVENT_HEAP_PROF_SAMPLE_BEGIN] = {
      .etNum = 162,
      .size = sizeof(StgWord64),
      .desc = "Start of heap profile sample"
    },
    [EVENT_HEAP_PROF_SAMPLE_COST_CENTRE] = {
      .etNum = 163,
      .size = 0xffff,
      .desc = "Heap profile cost-centre sample"
    },
    [EVENT_HEAP_PROF_SAMPLE_STRING] = {
      .etNum = 164,
      .size = 0xffff,
      .desc = "Heap profile string sample"
    },
    [EVENT_HEAP_PROF_SAMPLE_END] = {
      .etNum = 165,
      .size = sizeof(StgWord64),
      .desc = "End of heap profile sample"
    },
    [EVENT_HEAP_BIO_PROF_SAMPLE_BEGIN] = {
      .etNum = 166,
      .size = sizeof(StgWord64) + sizeof(StgWord64),
      .desc = "Start of heap profile (biographical) sample"
    },
    [EVENT_PROF_SAMPLE_COST_CENTRE] = {
      .etNum = 167,
      .size = 0xffff,
      .desc = "Time profile cost-centre stack"
    },
    [EVENT_PROF_BEGIN] = {
      .etNum = 168,
      .size = sizeof(StgWord64),
      .desc = "Start of a time profile"
    },
    [EVENT_IPE] = {
      .etNum = 169,
      .size = 0xffff,
      .desc = "An IPE entry"
    },
    [EVENT_USER_BINARY_MSG] = {
      .etNum = 181,
      .size = 0xffff,
      .desc = "User binary message"
    },
    [EVENT_CONC_MARK_BEGIN] = {
      .etNum = 200,
      .size = 0,
      .desc = "Begin concurrent mark phase"
    },
    [EVENT_CONC_MARK_END] = {
      .etNum = 201,
      .size = sizeof(StgWord32),
      .desc = "End concurrent mark phase"
    },
    [EVENT_CONC_SYNC_BEGIN] = {
      .etNum = 202,
      .size = 0,
      .desc = "Begin concurrent GC synchronisation"
    },
    [EVENT_CONC_SYNC_END] = {
      .etNum = 203,
      .size = 0,
      .desc = "End concurrent mark synchronisation"
    },
    [EVENT_CONC_SWEEP_BEGIN] = {
      .etNum = 204,
      .size = 0,
      .desc = "Begin concurrent sweep phase"
    },
    [EVENT_CONC_SWEEP_END] = {
      .etNum = 205,
      .size = 0,
      .desc = "End concurrent sweep phase"
    },
    [EVENT_CONC_UPD_REM_SET_FLUSH] = {
      .etNum = 206,
      .size = sizeof(EventCapNo),
      .desc = "Update remembered set flushed"
    },
    [EVENT_NONMOVING_HEAP_CENSUS] = {
      .etNum = 207,
      .size = sizeof(StgWord16) + sizeof(StgWord32) + sizeof(StgWord32) + sizeof(StgWord32),
      .desc = "Nonmoving heap census"
    },
    [EVENT_NONMOVING_PRUNED_SEGMENTS] = {
      .etNum = 208,
      .size = sizeof(StgWord32) + sizeof(StgWord32),
      .desc = "Report the amount of segments pruned and remaining on the free list."
    },
    [EVENT_TICKY_COUNTER_DEF] = {
      .etNum = 210,
      .size = 0xffff,
      .desc = "Ticky-ticky entry counter definition"
    },
    [EVENT_TICKY_COUNTER_SAMPLE] = {
      .etNum = 211,
      .size = sizeof(StgWord64) + sizeof(StgWord64) + sizeof(StgWord64) + sizeof(StgWord64),
      .desc = "Ticky-ticky entry counter sample"
    },
    [EVENT_TICKY_COUNTER_BEGIN_SAMPLE] = {
      .etNum = 212,
      .size = 0,
      .desc = "Ticky-ticky entry counter begin sample"
    },
};
