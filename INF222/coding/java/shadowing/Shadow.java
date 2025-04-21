package shadowing;

public class Shadow {
	public void a(){
		System.out.println("a");
	}
	public void b(){
		System.out.println("b");
	}
	private class innerClass{
		public void a(){
			System.out.println("innerClass a");
		}
		public void b(){
			Shadow.this.a();
			a();
		}
	}
	private void c(){
		innerClass inner = new innerClass();
		a();
		inner.a();
	}

}
