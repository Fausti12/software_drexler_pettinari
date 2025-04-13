package anillo;

import java.util.Stack;

public class Ring {
    private Stack<Link> stack = new Stack<>();

    private Link getPeekLink(){ return stack.peek(); }

    public Ring() {
        stack.push(new EmptyLink());
    }


    public Ring add(Object cargo) {
        getPeekLink().add(stack, cargo);
        return this;
    }

    public Ring next() {
        getPeekLink().next(stack);
        return this;

    }

    public Ring remove() {
        getPeekLink().remove(stack);
        return this;
    }

    public Object current() {
        return getPeekLink().current();
    }

}




