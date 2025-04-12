package anillo;

import java.util.Stack;

abstract class Link {
    public Object cargo;
    public abstract void add(Stack<Link> stack, Object cargo);
    public abstract void next(Stack<Link> stack);
    public abstract void remove(Stack<Link> stack);
    public abstract Object current();// hook
}

// Eslabón con datos
class LinkData extends Link {

    LinkData(Object cargo) { this.cargo = cargo; }

    public void add(Stack<Link> stack, Object cargo) {
        Link newLink = new LinkData(cargo);
        stack.push(newLink);
    }

    public void next(Stack<Link> stack) {
        Link current = stack.pop();
        stack.insertElementAt(current, 1); // se lo agrega al fondo
    }

    public void remove(Stack<Link> stack) { stack.pop();}

    public Object current() { return cargo; }

}

// Eslabón vacío
class EmptyLink extends Link {

    EmptyLink(){ cargo = null; }

    public void add(Stack<Link> stack, Object cargo) {
        stack.push(new LinkData(cargo));
    }

    public void next(Stack<Link> stack) {
        throw new RuntimeException("next() no permitido en anillo vacío");
    }

    public void remove(Stack<Link> stack) {
        throw new RuntimeException("remove() no permitido en anillo vacío");
    }

    public Object current() {throw new RuntimeException("current() no permitido en anillo vacío");}

}

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




