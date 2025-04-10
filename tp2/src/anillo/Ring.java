package anillo;

import java.util.Stack;

abstract class RingNode {
    public Object cargo;
    public RingNode next; //necesario para llamar métodos en ringnode
    public RingNode prev;
    public abstract RingNode add(Object cargo);
    public abstract RingNode next();
    public abstract RingNode remove();
    public abstract Object current();// hook
}

// Nodo con datos
class DataNode extends RingNode {

    DataNode(Object cargo) {
        this.cargo = cargo;
        this.next = this;
        this.prev = this;
    }

    public RingNode add(Object cargo) {
        RingNode newNode = new DataNode(cargo);           // Nuevo nodo con el valor
        newNode.next = this;
        this.prev.next = newNode;
        newNode.prev = this.prev;
        this.prev = newNode;
        return newNode;
    }

    public RingNode next() {
            return this.next;
    }

    public RingNode remove() {
        //copia info del siguiente nodo en actual
        this.cargo = this.next.cargo;
        this.next = this.next.next;
        this.prev = this.prev.prev;
        return this;
    }

    public Object current() {
        return cargo;
    }

}

// Nodo vacío
class EmptyNode extends RingNode {

    EmptyNode(){
        cargo = null;
        next = null;
        prev = null;}

    public RingNode add(Object cargo) {
        return new DataNode(cargo);

    }

    public RingNode next() {
        throw new RuntimeException("next() no permitido en anillo vacío");
    }

    public RingNode remove() {
        throw new RuntimeException("remove() no permitido en anillo vacío");
    }

    public Object current() {
        throw new RuntimeException("current() no permitido en anillo vacío");
    }

}

// Clase principal
public class Ring {
    private final Stack<RingNode> stack;

    public Ring() {
        this.stack = new Stack<>();
        this.stack.push(new EmptyNode());
    }


    public Ring add(Object cargo) {
        RingNode newNode = stack.peek().add(cargo);
        stack.push(newNode);
        return this;
    }

    public Ring next() {
        RingNode currentNode = stack.peek().next();
        stack.push(currentNode);
        stack.remove(1); //paso nodo en posicion 2 a arriba del stack, donde siempre esta el actual
        return this;

    }

    public Ring remove() {
        RingNode actNode = stack.peek().remove();
        stack.remove(1);
        return this;
    }

    public Object current() {
        return stack.peek().current(); // Puede lanzar excepcion si top es EmptyNode
    }

}




