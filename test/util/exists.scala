package made.util

inline def assumeExists[T](using inline ev: T): Unit = ()
