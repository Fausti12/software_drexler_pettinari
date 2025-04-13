package anillo;

abstract class Link {
    public abstract Link add(Object cargo);
    public abstract Link next();
    public abstract Link remove();
    public abstract Object current();
}

class DataLink extends Link {
    private Object cargo;
    private Link next;
    private  Link prev;

    DataLink(Object cargo) {
        this.cargo = cargo;
        this.next = this;
        this.prev = this;
    }

    public Link add(Object cargo) {
        DataLink newNode = new DataLink(cargo);
        newNode.setNext(this);
        newNode.setPrev((DataLink) this.getPrev());
        ((DataLink) this.getPrev()).setNext(newNode);
        this.setPrev(newNode);
        return newNode;
    }

    public Link next() {
        return next;
    }

    public Link remove(){
        this.cargo = this.next.current();
        this.next = ((DataLink) this.next).getNext();
        this.prev = ((DataLink) this.prev).getPrev();
        return this;
    }

    public Object current() {
        return cargo;
    }

    public Link getNext() {
        return next;
    }

    public Link getPrev() {
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
