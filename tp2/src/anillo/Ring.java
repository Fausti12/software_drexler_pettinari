package anillo;

abstract class RingNode {
    public Object cargo;
    public RingNode next;

    public abstract RingNode add(Object newCargo);
    public abstract RingNode next();
    public abstract RingNode remove();
    public abstract Object current();
}

class DataNode extends RingNode { // Nodo con datos

    public DataNode(Object cargo) {
        this.cargo = cargo;
        this.next = this; // Se apunta a sí mismo al principio
    }

    public RingNode add(Object newCargo) {
        RingNode newNode = new DataNode(this.cargo);
        newNode.next = this.next;
        this.cargo = newCargo;  // Nuevo nodo se agrega antes del actual
        this.next = newNode;
        return this;
    }

    public RingNode next() { return this.next; }

    public RingNode remove() {

        this.cargo = this.next.cargo;
        RingNode temp = this.next().next();
        this.next().next = new EmptyNode();
        try {
            this.next.current();
        } catch (RuntimeException e) {
            return new EmptyNode();
        }
        this.next().next = temp;
        return this;
    }

    public Object current() { return cargo; }
}

class EmptyNode extends RingNode { // Nodo vacío
    EmptyNode() {
        this.cargo = null;
        this.next = this; // Se apunta a sí mismo
    }

    public RingNode add(Object cargo) {
        return new DataNode(cargo); // Si es vacío, se convierte en un nodo real
    }

    public RingNode next() {
        throw new RuntimeException("Empty ring");
    }

    public RingNode remove() {
        throw new RuntimeException("Empty ring");
    }

    public Object current() {
        throw new RuntimeException("No value in ring");
    }
}

public class Ring {
    private RingNode node;

    public Ring() {
        node = new EmptyNode(); // Siempre empieza con un nodo vacío
    }

    public Ring add(Object cargo) {
        node = node.add(cargo);
        return this;
    }

    public Ring next() {
        node = node.next();
        return this;
    }

    public Object current() { return node.current(); }

    public Ring remove() {
        node = node.remove();
        return this;
    }
}