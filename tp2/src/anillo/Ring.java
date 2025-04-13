package anillo;

import java.util.Stack;

abstract class Link {
    public abstract Link add(Object cargo);
    public abstract Link next();
    public abstract Link remove();
    public abstract Object current();
}

class DataLink extends Link {
    private Object cargo;
    private DataLink next;
    private  DataLink prev;

    DataLink(Object cargo) {
        this.cargo = cargo;
        this.next = this;
        this.prev = this;
    }

    public DataLink add(Object cargo) {
        DataLink newNode = new DataLink(cargo);
        newNode.setNext(this);
        newNode.setPrev(this.getPrev());
        this.getPrev().setNext(newNode);
        this.setPrev(newNode);
        return newNode;
    }

    public DataLink next() {
        return next;
    }

    public DataLink remove(){
        this.cargo = this.next.current();
        this.next = this.next.getNext();
        this.prev = this.prev.getPrev();
        return this;
    }

    public Object current() {
        return cargo;
    }

    public DataLink getNext() {
        return next;
    }

    public DataLink getPrev() {
        return prev;
    }

    public void setNext(DataLink next) {
        this.next = next;
    }

    public void setPrev(DataLink prev) {
        this.prev = prev;
    }

}


class EmptyLink extends Link {

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