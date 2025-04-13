package anillo;

import java.util.Stack;

public class Ring {
    Stack<Link> stack = new Stack<Link>();
    public Ring(){
        stack.push(new EmptyLink());
    }

    public Ring add(Object cargo) {
        Link newLink = getPeekLink().add(cargo);
        stack.push(newLink);
        return this;
    }

    public Ring next() {
        Link currentNode = getPeekLink().next();
        stack.push(currentNode);
        stack.remove(1); //Se pasa el Link de abajo al tope del Stack
        return this;
    }

    public Ring remove() {
        getPeekLink().remove();
        stack.remove(1);
        return this;
    }

    public Object current() {
        return getPeekLink().current();
    }

    private Link getPeekLink(){
        return stack.peek();
    }


}