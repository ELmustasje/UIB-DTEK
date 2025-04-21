package org.example;

import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;

//Advice -> What  - @Before, @After ...
//PointCut -> Where/When

@Aspect
public class aspect {
	@Before("execution(public void show())")
	public void log(){
		System.out.println("log");
	}
}
