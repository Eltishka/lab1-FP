module ModuleTests

open NUnit.Framework
open ModuleTask1
open ModuleTask2

[<TestFixture>]
type ModuleTask1Test() =
    [<Test>]
    member _.FindMax_ReturnsCorrectValue() =
        let expected = 23514624000L
        let actual = int64 (findMax str)
        Assert.AreEqual(expected, actual)

[<TestFixture>]
type ModuleTask2Test() =
    [<Test>]
    member _.SumOfDivisors_WorksForKnownValue() = Assert.AreEqual(28, sumOfDivisors 28)

    [<Test>]
    member _.IsPresentedAsTwoAbundant_WorksForKnownValues() =
        Assert.IsTrue(isPresentedAsTwoAbundant 24)
        Assert.IsFalse(isPresentedAsTwoAbundant 3)

    [<Test>]
    member _.SumOfNotPresentedAsTwoAbundant_ReturnsCorrectValue() =
        let expected = 4179871
        let actual = sumOfNotPresentedAsTwoAbundant 28123
        Assert.AreEqual(expected, actual)
