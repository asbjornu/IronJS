// <auto-generated />
namespace IronJS.Tests.UnitTests.Sputnik.Conformance.NativeECMAScriptObjects.ArrayObjects.TheArrayConstructor
{
    using System;
    using NUnit.Framework;

    [TestFixture]
    public class NewArrayLenTests : SputnikTestFixture
    {
        public NewArrayLenTests()
            : base(@"Conformance\15_Native_ECMA_Script_Objects\15.4_Array_Objects\15.4.2_The_Array_Constructor\15.4.2.2_new_Array_len")
        {
        }

        [Test]
        [Category("Sputnik Conformance")]
        [Category("ECMA 15.2.4.2")]
        [Category("ECMA 15.2.4.6")]
        [Category("ECMA 15.4.2.2")]
        [TestCase("S15.4.2.2_A1.1_T1.js", Description = "The [[Prototype]] property of the newly constructed object is set to the original Array prototype object, the one that is the initial value of Array.prototype")]
        [TestCase("S15.4.2.2_A1.1_T2.js", Description = "The [[Prototype]] property of the newly constructed object is set to the original Array prototype object, the one that is the initial value of Array.prototype")]
        [TestCase("S15.4.2.2_A1.1_T3.js", Description = "The [[Prototype]] property of the newly constructed object is set to the original Array prototype object, the one that is the initial value of Array.prototype")]
        public void ThePrototypePropertyOfTheNewlyConstructedObjectIsSetToTheOriginalArrayPrototypeObjectTheOneThatIsTheInitialValueOfArrayPrototype(string file)
        {
            RunFile(file);
        }

        [Test]
        [Category("Sputnik Conformance")]
        [Category("ECMA 15.2.4.2")]
        [Category("ECMA 15.4.2.2")]
        [TestCase("S15.4.2.2_A1.2_T1.js", Description = "The [[Class]] property of the newly constructed object is set to \"Array\"")]
        public void TheClassPropertyOfTheNewlyConstructedObjectIsSetToArray(string file)
        {
            RunFile(file);
        }

        [Test]
        [Category("Sputnik Conformance")]
        [Category("ECMA 15.4.2.2")]
        [TestCase("S15.4.2.2_A2.1_T1.js", Description = "If the argument len is a Number and ToUint32(len) is equal to len, then the length property of the newly constructed object is set to ToUint32(len)")]
        public void IfTheArgumentLenIsANumberAndToUint32LenIsEqualToLenThenTheLengthPropertyOfTheNewlyConstructedObjectIsSetToToUint32Len(string file)
        {
            RunFile(file);
        }

        [Test]
        [Category("Sputnik Conformance")]
        [Category("ECMA 12.14")]
        [Category("ECMA 15.4.2.2")]
        [TestCase("S15.4.2.2_A2.2_T1.js", Description = "If the argument len is a Number and ToUint32(len) is not equal to len, a RangeError exception is thrown")]
        [TestCase("S15.4.2.2_A2.2_T2.js", Description = "If the argument len is a Number and ToUint32(len) is not equal to len, a RangeError exception is thrown")]
        [TestCase("S15.4.2.2_A2.2_T3.js", Description = "If the argument len is a Number and ToUint32(len) is not equal to len, a RangeError exception is thrown")]
        public void IfTheArgumentLenIsANumberAndToUint32LenIsNotEqualToLenARangeErrorExceptionIsThrown(string file)
        {
            RunFile(file);
        }

        [Test]
        [Category("Sputnik Conformance")]
        [Category("ECMA 15.4.2.2")]
        [TestCase("S15.4.2.2_A2.3_T1.js", Description = "If the argument len is not a Number, then the length property of the newly constructed object is set to 1 and the 0 property of the newly constructed object is set to len")]
        [TestCase("S15.4.2.2_A2.3_T2.js", Description = "If the argument len is not a Number, then the length property of the newly constructed object is set to 1 and the 0 property of the newly constructed object is set to len")]
        [TestCase("S15.4.2.2_A2.3_T3.js", Description = "If the argument len is not a Number, then the length property of the newly constructed object is set to 1 and the 0 property of the newly constructed object is set to len")]
        [TestCase("S15.4.2.2_A2.3_T4.js", Description = "If the argument len is not a Number, then the length property of the newly constructed object is set to 1 and the 0 property of the newly constructed object is set to len")]
        [TestCase("S15.4.2.2_A2.3_T5.js", Description = "If the argument len is not a Number, then the length property of the newly constructed object is set to 1 and the 0 property of the newly constructed object is set to len")]
        public void IfTheArgumentLenIsNotANumberThenTheLengthPropertyOfTheNewlyConstructedObjectIsSetTo1AndThe0PropertyOfTheNewlyConstructedObjectIsSetToLen(string file)
        {
            RunFile(file);
        }
    }
}