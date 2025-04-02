package anillo;

class Nodo { //cambiar nombre a clase
    Object cargo;
    Nodo next;

    Nodo(Object cargo) {
        this.cargo = cargo;
        this.next = this; // Se apunta a sí mismo al principio
    }

    Nodo add(Object newCargo) {
        Nodo newNode = new Nodo(this.cargo);
        newNode.next = this.next;
        this.cargo = newCargo;  //nuevo nodo se agrega antes del actual
        this.next = newNode;
        return this;
    }

    Nodo next() { return this.next; }

    Nodo remove() {
        return (this.next == this) ? new NodoVacio() : this.next; // ✅ Si hay un solo nodo, vuelve a NodoVacio
    }

    Object current() { return cargo; }
}

class NodoVacio extends Nodo {
    NodoVacio() {
        super(null); // se pasa cargo=null a constructor Nodo
        this.next = this; // Se apunta a sí mismo
    }

    @Override //redefinir un metodo de la clase padre en una subclase
    Nodo add(Object cargo) {
        return new Nodo(cargo); // Si es vacío, se convierte en un nodo real
    }

    @Override
    Nodo next() {
        throw new RuntimeException("Empty ring");
    }

    @Override
    Nodo remove() {
        throw new RuntimeException("Empty ring");
    }

    @Override
    Object current() {
        throw new RuntimeException("No value in ring");
    }
}

public class Ring {
    private Nodo nodo;

    public Ring() {
        nodo = new NodoVacio(); // Siempre empieza con un nodo vacío
    }

    public Ring add(Object cargo) {
        nodo = nodo.add(cargo);
        return this;
    }

    public Ring next() {
        //nodo = nodo.next;
        nodo = nodo.next();
        return this;
    }

    public Object current() { return nodo.current(); }

    public Ring remove() {
        nodo = nodo.remove();
        return this;
    }
}

