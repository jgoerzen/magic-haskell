/* arch-tag: Python Utility Functions
 */

#include <Python.h>

void pyhs_DECREF(PyObject *o) {
  Py_DECREF(o);
}

