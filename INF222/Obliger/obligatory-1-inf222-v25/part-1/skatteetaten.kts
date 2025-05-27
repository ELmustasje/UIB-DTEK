
abstract class Taxpayer(val id: String){
    override fun toString(): String{
        return id
    }
}
class Employee(id: String, val salary: Double) : Taxpayer(id)
class Business(id: String, val revenue: Double, val expenses: Double) : Taxpayer(id)
class Freelancer(id: String, val projects: List<Double>) : Taxpayer(id)

fun interface TaxRule<in T : Taxpayer> {
    fun calculateTax(taxpayer: T): Double
}

class TaxOffice<T : Taxpayer>{
    private val taxRules = mutableListOf<TaxRule<T>>()
    private val taxpayers = mutableListOf<T>()

    fun registerTaxpayer(Taxpayer: T){
        taxpayers.add(Taxpayer)
    }

    fun assignTaxRule(TaxRule: TaxRule<T>){
        taxRules.add(TaxRule)
    }

    fun calculateTaxes(): Map<String, Double> {
        val taxresults = mutableMapOf<String, Double>()
        for (taxpayer in taxpayers){
            var total = 0.0;
            for (taxRule in taxRules){
                total += taxRule.calculateTax(taxpayer);
            }
            taxresults[taxpayer.id] = total;
        }
        return taxresults;
    }
}

class Skatteetaten {
    private val offices = mutableMapOf<String, TaxOffice<out Taxpayer>>();
    fun addOffice(name: String, office: TaxOffice<out Taxpayer>){
        offices[name] = office;
    }
    fun getOffices(): Map<String, TaxOffice<out Taxpayer>>{
        return offices.toMap();
    }
}

fun main(){
    // Taxpayers
    val employee1 = Employee("E001", 500_000.0)
    val employee2 = Employee("E002", 600_000.0)

    val business1 = Business("B001", 1_000_000.0, 400_000.0)
    val business2 = Business("B002", 2_000_000.0, 1_200_000.0)

    val freelancer1 = Freelancer("F001", listOf(50_000.0, 80_000.0))
    val freelancer2 = Freelancer("F002", listOf(100_000.0, 200_000.0))

    // Tax Rules
    // flat fee of 10 NOK for any income earner
    val generalTaxRule = TaxRule<Taxpayer> { _ -> 10.0 } 

    // 30% tax on salary
    val employeeTaxRule = TaxRule<Employee> { taxpayer -> taxpayer.salary * 0.3 }
    
    // 20% tax on profit
    val businessTaxRule = TaxRule<Business> {taxpayer ->  (taxpayer.revenue - taxpayer.expenses) * 0.2  } 

    // 25% tax on total project income
    val freelancerTaxRule = TaxRule<Freelancer> { taxpayer -> taxpayer.projects.sum() * 0.25 } 
    
    // Tax Offices
    val employeeOffice = TaxOffice<Employee>().apply {
        registerTaxpayer(employee1)
        registerTaxpayer(employee2)
        assignTaxRule(employeeTaxRule)
        assignTaxRule(generalTaxRule)
    }
    
    val businessOffice = TaxOffice<Business>().apply {
        registerTaxpayer(business1)
        registerTaxpayer(business2)
        assignTaxRule(businessTaxRule)
        assignTaxRule(generalTaxRule)
    }
    
    val freelancerOffice = TaxOffice<Freelancer>().apply {
        registerTaxpayer(freelancer1)
        registerTaxpayer(freelancer2)
        assignTaxRule(freelancerTaxRule)
        assignTaxRule(generalTaxRule)
    }

    // Skatteetaten

    val skatteetaten = Skatteetaten().apply {
        addOffice("Employees", employeeOffice)
        addOffice("Businesses", businessOffice)
        addOffice("Freelancers", freelancerOffice)
    }

    // Calculate Taxes
    for (office in skatteetaten.getOffices()){
        println("${office.key} taxes: ${office.value.calculateTaxes()}")
    }
}

main()

// Expected output:

// Employees taxes: {E001=150010.0, E002=180010.0}
// Businesses taxes: {B001=120010.0, B002=160010.0}
// Freelancers taxes: {F001=32510.0, F002=75010.0}



