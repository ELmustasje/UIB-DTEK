package inf222.aop;

import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.aspectj.lang.reflect.FieldSignature;

import java.lang.reflect.Field;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Aspect
public class CurrencyAspect {

/*
	@Before("execution(* Arithmetics.runExample())")
	public void convert(JoinPoint joinPoint) throws IllegalAccessException {
		Field[] a = joinPoint.getTarget().getClass().getDeclaredFields();
		Pattern pattern = Pattern.compile("[a-zA-Z]_(\\w+)");


		if(a.length > 0){
			for (Field field : a) {
				field.setAccessible(true);
				if(field.getType() == double.class){
					Matcher matcher = pattern.matcher(field.getName());
					if(matcher.find()){
						String currency = matcher.group(1);
						switch(currency){
							case "GBP":
								field.setDouble(joinPoint.getTarget(), field.getDouble(joinPoint.getTarget()) * 14);
								break;
							case "USD":
								field.setDouble(joinPoint.getTarget(), field.getDouble(joinPoint.getTarget()) * 11);
								break;
							case "JPY":
								field.setDouble(joinPoint.getTarget(), field.getDouble(joinPoint.getTarget()) * 0.07);
								break;
							case "DKK":
								field.setDouble(joinPoint.getTarget(), field.getDouble(joinPoint.getTarget()) * 1.5);
								break;
						}
					}
				}
			}


	}
*/
	@Around("get(double inf222..*)")
	public double convertedToNok(ProceedingJoinPoint joinPoint) throws Throwable {
		String name = joinPoint.getSignature().getName();
		double val = (double) joinPoint.proceed();
		return coonvertToNok(val, name);
	}
	@Around("set(* inf222..*) && !cflow(execution(inf222..*.new(..)))")
	public void convertBack(ProceedingJoinPoint joinPoint) throws Throwable {
		String name = joinPoint.getSignature().getName();
		Object[] args = joinPoint.getArgs();

		if(args.length > 0){
			double val = (double) args[0];
			checkPositive(val);
			double newVal = coonverBack(val, name);
			joinPoint.proceed(new Object[]{newVal});
		}

		joinPoint.proceed(new Object[]{});

		//return coonverBack(val, name);
	}

	private void checkPositive(double val) {
		if(val < 0){
			throw new IllegalArgumentException("Negative numbers are not allowed");
		}
	}

	private double coonvertToNok(double val, String name) {
		if(name.endsWith("DKK")){
			return val * 1.5;
		}
		if(name.endsWith("GBP")){
			return val * 14;
		}
		if(name.endsWith("USD")){
			return val * 11;
		}
		if(name.endsWith("JPY")){
			return val * 0.07;
		}
		return val;
	}

	private double coonverBack(double val, String name) {
		if(name.endsWith("DKK")){
			return val / 1.5;
		}
		if(name.endsWith("GBP")){
			return val / 14;
		}
		if(name.endsWith("USD")){
			return val / 11;
		}
		if(name.endsWith("JPY")){
			return val / 0.07;
		}
		return val;
	}

}