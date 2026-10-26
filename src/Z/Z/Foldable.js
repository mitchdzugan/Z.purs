export const js_withNewInds = (mkTuple) => (arr) => (toNewLifted) => {
    let head = null;
    let prev = null;
    const lifteds = [];
    for (const el of arr) {
        const curr = { el }
        head ||= curr;
        if (prev) {
            prev.next = curr;
        }
        prev = curr;
        lifteds.push(curr);
    }
    const newLifted = toNewLifted(lifteds);
    let ind = 0;
    for (const wrapped of newLifted) {
        wrapped.ind = ind;
        ind++;
    }
    const res = [];
    let curr = head;
    while (curr) {
        res.push(mkTuple(curr.ind)(curr.el));
        curr = curr.next;
    }
    console.log({ res })
    return res;
};