#pragma once

// When adding a new event type used by GHC you should also update
// NUM_GHC_EVENT_TAGS in include/rts/EventLogFormat.h.

EventType eventTypes[] = {
    [EVENT_CREATE_THREAD] = {
      .etNum = EVENT_CREATE_THREAD,
      .size = sizeof(EventThreadID),
      .desc = "Create thread"
    },
    [EVENT_RUN_THREAD] = {
      .etNum = EVENT_RUN_THREAD,
      .size = sizeof(EventThreadID),
      .desc = "Run thread"
    },
    [EVENT_STOP_THREAD] = {
      .etNum = EVENT_STOP_THREAD,
      .size = sizeof(EventThreadID) + sizeof(StgWord16) + sizeof(EventThreadID),
      .desc = "Stop thread"
    },
    [EVENT_THREAD_RUNNABLE] = {
      .etNum = EVENT_THREAD_RUNNABLE,
      .size = sizeof(EventThreadID),
      .desc = "Thread runnable"
    },
    [EVENT_MIGRATE_THREAD] = {
      .etNum = EVENT_MIGRATE_THREAD,
      .size = sizeof(EventThreadID) + sizeof(EventCapNo),
      .desc = "Migrate thread"
    },
    [EVENT_THREAD_WAKEUP] = {
      .etNum = EVENT_THREAD_WAKEUP,
      .size = sizeof(EventThreadID) + sizeof(EventCapNo),
      .desc = "Wakeup thread"
    },
    [EVENT_GC_START] = {
      .etNum = EVENT_GC_START,
      .size = 0,
      .desc = "Starting GC"
    },
    [EVENT_GC_END] = {
      .etNum = EVENT_GC_END ,
      .size = 0,
      .desc = "Finished GC"
    },
    [EVENT_REQUEST_SEQ_GC] = {
      .etNum = EVENT_REQUEST_SEQ_GC ,
      .size = 0,
      .desc = "Request sequential GC"
    },
    [EVENT_REQUEST_PAR_GC] = {
      .etNum = EVENT_REQUEST_PAR_GC ,
      .size = 0,
      .desc = "Request parallel GC"
    },
    [EVENT_CREATE_SPARK_THREAD] = {
      .etNum = EVENT_CREATE_SPARK_THREAD ,
      .size = sizeof(EventThreadID),
      .desc = "Create spark thread"
    },
    [EVENT_LOG_MSG] = {
      .etNum = EVENT_LOG_MSG ,
      .size = 0xffff,
      .desc = "Log message"
    },
    [EVENT_BLOCK_MARKER] = {
      .etNum = EVENT_BLOCK_MARKER ,
      .size = sizeof(StgWord32) + sizeof(EventTimestamp) + sizeof(EventCapNo),
      .desc = "Block marker"
    },
    [EVENT_USER_MSG] = {
      .etNum = EVENT_USER_MSG ,
      .size = 0xffff,
      .desc = "User message"
    },
    [EVENT_GC_IDLE] = {
      .etNum = EVENT_GC_IDLE ,
      .size = 0,
      .desc = "GC idle"
    },
    [EVENT_GC_WORK] = {
      .etNum = EVENT_GC_WORK ,
      .size = 0,
      .desc = "GC working"
    },
    [EVENT_GC_DONE] = {
      .etNum = EVENT_GC_DONE ,
      .size = 0,
      .desc = "GC done"
    },
    [EVENT_CAPSET_CREATE] = {
      .etNum = EVENT_CAPSET_CREATE ,
      .size = sizeof(EventCapsetID) + sizeof(EventCapsetType),
      .desc = "Create capability set"
    },
    [EVENT_CAPSET_DELETE] = {
      .etNum = EVENT_CAPSET_DELETE ,
      .size = sizeof(EventCapsetID),
      .desc = "Delete capability set"
    },
    [EVENT_CAPSET_ASSIGN_CAP] = {
      .etNum = EVENT_CAPSET_ASSIGN_CAP ,
      .size = sizeof(EventCapsetID) + sizeof(EventCapNo),
      .desc = "Add capability to capability set"
    },
    [EVENT_CAPSET_REMOVE_CAP] = {
      .etNum = EVENT_CAPSET_REMOVE_CAP ,
      .size = sizeof(EventCapsetID) + sizeof(EventCapNo),
      .desc = "Remove capability from capability set"
    },
    [EVENT_RTS_IDENTIFIER] = {
      .etNum = EVENT_RTS_IDENTIFIER ,
      .size = 0xffff,
      .desc = "RTS name and version"
    },
    [EVENT_PROGRAM_ARGS] = {
      .etNum = EVENT_PROGRAM_ARGS ,
      .size = 0xffff,
      .desc = "Program arguments"
    },
    [EVENT_PROGRAM_ENV] = {
      .etNum = EVENT_PROGRAM_ENV ,
      .size = 0xffff,
      .desc = "Program environment variables"
    },
    [EVENT_OSPROCESS_PID] = {
      .etNum = EVENT_OSPROCESS_PID ,
      .size = sizeof(EventCapsetID) + sizeof(StgWord32),
      .desc = "Process ID"
    },
    [EVENT_OSPROCESS_PPID] = {
      .etNum = EVENT_OSPROCESS_PPID ,
      .size = sizeof(EventCapsetID) + sizeof(StgWord32),
      .desc = "Parent process ID"
    },
    [EVENT_SPARK_COUNTERS] = {
      .etNum = EVENT_SPARK_COUNTERS ,
      .size = sizeof(StgWord64) + sizeof(StgWord64) + sizeof(StgWord64) + sizeof(StgWord64) + sizeof(StgWord64) + sizeof(StgWord64) + sizeof(StgWord64),
      .desc = "Spark counters"
    },
    [EVENT_SPARK_CREATE] = {
      .etNum = EVENT_SPARK_CREATE ,
      .size = 0,
      .desc = "Spark create"
    },
    [EVENT_SPARK_DUD] = {
      .etNum = EVENT_SPARK_DUD ,
      .size = 0,
      .desc = "Spark dud"
    },
    [EVENT_SPARK_OVERFLOW] = {
      .etNum = EVENT_SPARK_OVERFLOW ,
      .size = 0,
      .desc = "Spark overflow"
    },
    [EVENT_SPARK_RUN] = {
      .etNum = EVENT_SPARK_RUN ,
      .size = 0,
      .desc = "Spark run"
    },
    [EVENT_SPARK_STEAL] = {
      .etNum = EVENT_SPARK_STEAL ,
      .size = sizeof(EventCapNo),
      .desc = "Spark steal"
    },
    [EVENT_SPARK_FIZZLE] = {
      .etNum = EVENT_SPARK_FIZZLE ,
      .size = 0,
      .desc = "Spark fizzle"
    },
    [EVENT_SPARK_GC] = {
      .etNum = EVENT_SPARK_GC ,
      .size = 0,
      .desc = "Spark GC"
    },
    [EVENT_INTERN_STRING] = {
      .etNum = EVENT_INTERN_STRING ,
      .size = 0xffff,
      .desc = "Intern string"
    },
    [EVENT_WALL_CLOCK_TIME] = {
      .etNum = EVENT_WALL_CLOCK_TIME ,
      .size = sizeof(EventCapsetID) + sizeof(StgWord64) + sizeof(StgWord32),
      .desc = "Wall clock time"
    },
    [EVENT_THREAD_LABEL] = {
      .etNum = EVENT_THREAD_LABEL ,
      .size = 0xffff,
      .desc = "Thread label"
    },
    [EVENT_CAP_CREATE] = {
      .etNum = EVENT_CAP_CREATE ,
      .size = sizeof(EventCapNo),
      .desc = "Create capability"
    },
    [EVENT_CAP_DELETE] = {
      .etNum = EVENT_CAP_DELETE ,
      .size = sizeof(EventCapNo),
      .desc = "Delete capability"
    },
    [EVENT_CAP_DISABLE] = {
      .etNum = EVENT_CAP_DISABLE ,
      .size = sizeof(EventCapNo),
      .desc = "Disable capability"
    },
    [EVENT_CAP_ENABLE] = {
      .etNum = EVENT_CAP_ENABLE ,
      .size = sizeof(EventCapNo),
      .desc = "Enable capability"
    },
    [EVENT_HEAP_ALLOCATED] = {
      .etNum = EVENT_HEAP_ALLOCATED ,
      .size = sizeof(EventCapsetID) + sizeof(StgWord64),
      .desc = "Total heap memory ever allocated"
    },
    [EVENT_HEAP_SIZE] = {
      .etNum = EVENT_HEAP_SIZE ,
      .size = sizeof(EventCapsetID) + sizeof(StgWord64),
      .desc = "Current heap size (number of allocated mblocks)"
    },
    [EVENT_HEAP_LIVE] = {
      .etNum = EVENT_HEAP_LIVE ,
      .size = sizeof(EventCapsetID) + sizeof(StgWord64),
      .desc = "Current heap live data"
    },
    [EVENT_HEAP_INFO_GHC] = {
      .etNum = EVENT_HEAP_INFO_GHC ,
      .size = sizeof(EventCapsetID) + sizeof(StgWord16) + sizeof(StgWord64) + sizeof(StgWord64) + sizeof(StgWord64) + sizeof(StgWord64),
      .desc = "Heap static parameters"
    },
    [EVENT_GC_STATS_GHC] = {
      .etNum = EVENT_GC_STATS_GHC ,
      .size = sizeof(EventCapsetID) + sizeof(StgWord16) + sizeof(StgWord64) + sizeof(StgWord64) + sizeof(StgWord64) + sizeof(StgWord32) + sizeof(StgWord64) + sizeof(StgWord64) + sizeof(StgWord64),
      .desc = "GC statistics"
    },
    [EVENT_GC_GLOBAL_SYNC] = {
      .etNum = EVENT_GC_GLOBAL_SYNC ,
      .size = 0,
      .desc = "Synchronise stop-the-world GC"
    },
    [EVENT_TASK_CREATE] = {
      .etNum = EVENT_TASK_CREATE ,
      .size = sizeof(EventTaskId) + sizeof(EventCapNo) + sizeof(EventKernelThreadId),
      .desc = "Task create"
    },
    [EVENT_TASK_MIGRATE] = {
      .etNum = EVENT_TASK_MIGRATE ,
      .size = sizeof(EventTaskId) + sizeof(EventCapNo) + sizeof(EventCapNo),
      .desc = "Task migrate"
    },
    [EVENT_TASK_DELETE] = {
      .etNum = EVENT_TASK_DELETE ,
      .size = sizeof(EventTaskId),
      .desc = "Task delete"
    },
    [EVENT_USER_MARKER] = {
      .etNum = EVENT_USER_MARKER ,
      .size = 0xffff,
      .desc = "User marker"
    },
    [EVENT_HACK_BUG_T9003] = {
      .etNum = EVENT_HACK_BUG_T9003 ,
      .size = 0,
      .desc = "Empty event for bug #9003"
    },
    [EVENT_MEM_RETURN] = {
      .etNum = EVENT_MEM_RETURN ,
      .size = sizeof(EventCapsetID) + sizeof(StgWord32) + sizeof(StgWord32) + sizeof(StgWord32),
      .desc = "The RTS attempted to return heap memory to the OS"
    },
    [EVENT_BLOCKS_SIZE] = {
      .etNum = EVENT_BLOCKS_SIZE ,
      .size = sizeof(EventCapsetID) + sizeof(StgWord64),
      .desc = "Report the size of the heap in blocks"
    },
    [EVENT_HEAP_PROF_BEGIN] = {
      .etNum = EVENT_HEAP_PROF_BEGIN,
      .size = 0xffff,
      .desc = "Start of heap profile"
    },
    [EVENT_HEAP_PROF_COST_CENTRE] = {
      .etNum = EVENT_HEAP_PROF_COST_CENTRE,
      .size = 0xffff,
      .desc = "Cost-centre definition"
    },
    [EVENT_HEAP_PROF_SAMPLE_BEGIN] = {
      .etNum = EVENT_HEAP_PROF_SAMPLE_BEGIN,
      .size = sizeof(StgWord64),
      .desc = "Start of heap profile sample"
    },
    [EVENT_HEAP_PROF_SAMPLE_COST_CENTRE] = {
      .etNum = EVENT_HEAP_PROF_SAMPLE_COST_CENTRE,
      .size = 0xffff,
      .desc = "Heap profile cost-centre sample"
    },
    [EVENT_HEAP_PROF_SAMPLE_STRING] = {
      .etNum = EVENT_HEAP_PROF_SAMPLE_STRING,
      .size = 0xffff,
      .desc = "Heap profile string sample"
    },
    [EVENT_HEAP_PROF_SAMPLE_END] = {
      .etNum = EVENT_HEAP_PROF_SAMPLE_END,
      .size = sizeof(StgWord64),
      .desc = "End of heap profile sample"
    },
    [EVENT_HEAP_BIO_PROF_SAMPLE_BEGIN] = {
      .etNum = EVENT_HEAP_BIO_PROF_SAMPLE_BEGIN,
      .size = sizeof(StgWord64) + sizeof(StgWord64),
      .desc = "Start of heap profile (biographical) sample"
    },
    [EVENT_PROF_SAMPLE_COST_CENTRE] = {
      .etNum = EVENT_PROF_SAMPLE_COST_CENTRE,
      .size = 0xffff,
      .desc = "Time profile cost-centre stack"
    },
    [EVENT_PROF_BEGIN] = {
      .etNum = EVENT_PROF_BEGIN,
      .size = sizeof(StgWord64),
      .desc = "Start of a time profile"
    },
    [EVENT_IPE] = {
      .etNum = EVENT_IPE,
      .size = 0xffff,
      .desc = "An IPE entry"
    },
    [EVENT_USER_BINARY_MSG] = {
      .etNum = EVENT_USER_BINARY_MSG,
      .size = 0xffff,
      .desc = "User binary message"
    },
    [EVENT_CONC_MARK_BEGIN] = {
      .etNum = EVENT_CONC_MARK_BEGIN,
      .size = 0,
      .desc = "Begin concurrent mark phase"
    },
    [EVENT_CONC_MARK_END] = {
      .etNum = EVENT_CONC_MARK_END,
      .size = sizeof(StgWord32),
      .desc = "End concurrent mark phase"
    },
    [EVENT_CONC_SYNC_BEGIN] = {
      .etNum = EVENT_CONC_SYNC_BEGIN,
      .size = 0,
      .desc = "Begin concurrent GC synchronisation"
    },
    [EVENT_CONC_SYNC_END] = {
      .etNum = EVENT_CONC_SYNC_END,
      .size = 0,
      .desc = "End concurrent mark synchronisation"
    },
    [EVENT_CONC_SWEEP_BEGIN] = {
      .etNum = EVENT_CONC_SWEEP_BEGIN,
      .size = 0,
      .desc = "Begin concurrent sweep phase"
    },
    [EVENT_CONC_SWEEP_END] = {
      .etNum = EVENT_CONC_SWEEP_END,
      .size = 0,
      .desc = "End concurrent sweep phase"
    },
    [EVENT_CONC_UPD_REM_SET_FLUSH] = {
      .etNum = EVENT_CONC_UPD_REM_SET_FLUSH,
      .size = sizeof(EventCapNo),
      .desc = "Update remembered set flushed"
    },
    [EVENT_NONMOVING_HEAP_CENSUS] = {
      .etNum = EVENT_NONMOVING_HEAP_CENSUS,
      .size = sizeof(StgWord16) + sizeof(StgWord32) + sizeof(StgWord32) + sizeof(StgWord32),
      .desc = "Nonmoving heap census"
    },
    [EVENT_NONMOVING_PRUNED_SEGMENTS] = {
      .etNum = EVENT_NONMOVING_PRUNED_SEGMENTS,
      .size = sizeof(StgWord32) + sizeof(StgWord32),
      .desc = "Report the amount of segments pruned and remaining on the free list."
    },
    [EVENT_TICKY_COUNTER_DEF] = {
      .etNum = EVENT_TICKY_COUNTER_DEF,
      .size = 0xffff,
      .desc = "Ticky-ticky entry counter definition"
    },
    [EVENT_TICKY_COUNTER_SAMPLE] = {
      .etNum = EVENT_TICKY_COUNTER_SAMPLE,
      .size = sizeof(StgWord64) + sizeof(StgWord64) + sizeof(StgWord64) + sizeof(StgWord64),
      .desc = "Ticky-ticky entry counter sample"
    },
    [EVENT_TICKY_COUNTER_BEGIN_SAMPLE] = {
      .etNum = EVENT_TICKY_COUNTER_BEGIN_SAMPLE,
      .size = 0,
      .desc = "Ticky-ticky entry counter begin sample"
    },
};
