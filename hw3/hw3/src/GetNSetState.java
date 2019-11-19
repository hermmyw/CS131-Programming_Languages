import java.util.concurrent.atomic.AtomicIntegerArray;
class GetNSetState implements State {
	private AtomicIntegerArray value;
	private byte maxval;
	
	GetNSetState(byte[] v) {
		value = new AtomicIntegerArray(v.length);
		for (int i = 0; i < v.length; i++) {
			value.set(i, (int) v[i]);
		}
		maxval = 127;
	}
	GetNSetState(byte[] v, byte m) {
		value = new AtomicIntegerArray(v.length);
		for (int i = 0; i < v.length; i++) {
			value.set(i, (int) v[i]);
		}
		maxval = m;
	}
	
    public int size() { return value.length(); }
    
    public byte[] current() {
    	byte[] curr = new byte[value.length()];
    	for (int i = 0; i < value.length(); i++) {
			curr[i] = (byte) value.get(i);
		}
    	return curr;
    }
    
    public boolean swap(int i, int j) {
    	int val_i = value.get(i);
    	int val_j = value.get(j);
    	if (val_i <= 0 || val_j >= maxval) {
    	    return false;
    	}
    	value.set(i, val_i-1);
    	value.set(j, val_j+1);
    	return true;
    }
}
