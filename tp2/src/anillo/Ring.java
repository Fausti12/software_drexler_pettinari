package anillo;

import java.util.Stack;

abstract class Link {
    public Object cargo;
    public Link next; //necesario para llamar métodos en ringnode
    public Link prev;

    public abstract Link add(Object cargo);

    public abstract Link next();

    public abstract Link remove();

    public abstract Object current();

}

class DataLink extends Link {
    DataLink(Object cargo) {
        this.cargo = cargo;
        this.next = this;
        this.prev = this;
    }

    public Link add(Object cargo) {
        Link newNode = new DataLink(cargo);           // Nuevo nodo con el valor
        newNode.next = this;
        this.prev.next = newNode;
        newNode.prev = this.prev;
        this.prev = newNode;
        return newNode;
    }

    public Link next() {
        return next;
    }

    public Link remove(){
        this.cargo = this.next.cargo;
        this.next = this.next.next;
        this.prev = this.prev.prev;
        return this;
    }

    public Object current() {
        return cargo;
    }

}


class EmptyLink extends Link {
    EmptyLink(){
        cargo = null;
        next = null;
        prev = null;
    }

    public Link add(Object cargo) {
        return new DataLink(cargo);
    }

    public Link next() {
        throw new RuntimeException("next() no permitido en anillo vacío");
    }

    public Link remove() {
        throw new RuntimeException("remove() no permitido en anillo vacío");
    }

    public Object current() {
        throw new RuntimeException("current() no permitido en anillo vacío");
    }

}



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
        stack.remove(1); //paso nodo en posicion 2 a arriba del stack, donde siempre está el actual
        return this;
    }

    public Ring remove() {
        Link actNode = getPeekLink().remove();
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