const assert = condition => {
  if (!condition) {
    throw new Error("AssertionError");
  }
};
