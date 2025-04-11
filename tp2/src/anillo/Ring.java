package anillo;

import java.util.Stack;

abstract class Link {
    public Object cargo;
    public abstract Link add(Stack<Link> stack, Object cargo);
    public abstract Link next(Stack<Link> stack);
    public abstract Link remove(Stack<Link> stack);
    public abstract Object current(Stack<Link> stack);// hook
}

// Nodo con datos
class LinkData extends Link {

    LinkData(Object cargo) {
        this.cargo = cargo;
    }

    public Link add(Stack<Link> stack, Object cargo) {
        Link newLink = new LinkData(cargo);
        stack.push(newLink);
        return newLink; //esto no haría falta -> return Null
    }

    public Link next(Stack<Link> stack) {
        Link current = stack.pop();
        stack.insertElementAt(current, 1); // lo mandás al fondo
        return stack.peek();
    }

    public Link remove(Stack<Link> stack) {
        stack.pop();
        Link nextLink = stack.peek();
        return nextLink; //no haría falta
    }

    public Object current(Stack<Link> stack) {
        return cargo;
    }

}

// Nodo vacío
class EmptyNode extends Link {

    EmptyNode(){
        cargo = null;}

    public Link add(Stack<Link> stack, Object cargo) {
        stack.push(new LinkData(cargo));
        return stack.peek();
    }

    public Link next(Stack<Link> stack) {
        throw new RuntimeException("next() no permitido en anillo vacío");
    }

    public Link remove(Stack<Link> stack) {
        throw new RuntimeException("remove() no permitido en anillo vacío");
    }

    public Object current(Stack<Link> stack) {throw new RuntimeException("current() no permitido en anillo vacío");}

}

// Clase principal
public class Ring {
    private Stack<Link> stack = new Stack<>();
    private Link currentLink; //tal vez no haga falta: métodos de Link devuelven Null y current sería el peek del stack

    public Ring() {
        stack.push(new EmptyNode());
        currentLink = stack.getFirst();
    }


    public Ring add(Object cargo) {
        currentLink = currentLink.add(stack, cargo);
        return this;
    }

    public Ring next() {
        currentLink = currentLink.next(stack);
        return this;

    }

    public Ring remove() {
        currentLink = currentLink.remove(stack);
        return this;
    }

    public Object current() {
        return currentLink.current(stack);
    }

}




