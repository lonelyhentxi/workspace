using System;
using NUnit.Framework;
using System.Linq;
using System.Text;

namespace Codewars.Sixkyu
{
    public class SimpleEncryption1AlternatingSplit
    {
        public static string Encrypt(string text, int n)
        {
            if (text is null)
            {
                return null;
            }

            var currentText = text;

            for (var i = 0; i < n; i++)
            {
                currentText = String.Join("",
                    currentText.Where((ch, index) => index % 2 == 1)
                        .Concat(currentText.Where((ch, index) => index % 2 == 0)));
            }

            return currentText;
        }

        public static string Decrypt(string encryptedText, int n)
        {
            if (encryptedText is null)
            {
                return null;
            }

            var currentText = encryptedText;
            for (var i = 0; i < n; i++)
            {
                var midSize = (int) Math.Ceiling(currentText.Length / 2.0);
                var midIndex = currentText.Length - midSize;
                var front = currentText.Substring(0, midIndex);
                var back = currentText.Substring(midIndex, midSize);
                var build = new StringBuilder();
                for (var j = 0; j < midIndex; j++)
                {
                    build.Append(back[j]);
                    build.Append(front[j]);
                }

                if (currentText.Length % 2 == 1)
                {
                    build.Append(back[back.Length - 1]);
                }

                currentText = build.ToString();
            }

            return currentText;
        }
    }

    public class SimpleEncryption1AlternatingSplitTest
    {
        [Test]
        public void EncryptExampleTests()
        {
            Assert.AreEqual("This is a test!", SimpleEncryption1AlternatingSplit.Encrypt("This is a test!", 0));
            Assert.AreEqual("hsi  etTi sats!", SimpleEncryption1AlternatingSplit.Encrypt("This is a test!", 1));
            Assert.AreEqual("s eT ashi tist!", SimpleEncryption1AlternatingSplit.Encrypt("This is a test!", 2));
            Assert.AreEqual(" Tah itse sits!", SimpleEncryption1AlternatingSplit.Encrypt("This is a test!", 3));
            Assert.AreEqual("This is a test!", SimpleEncryption1AlternatingSplit.Encrypt("This is a test!", 4));
            Assert.AreEqual("This is a test!", SimpleEncryption1AlternatingSplit.Encrypt("This is a test!", -1));
        }

        [Test]
        public void DecryptExampleTests()
        {
            Assert.AreEqual("This is a test!", SimpleEncryption1AlternatingSplit.Decrypt("This is a test!", 0));
            Assert.AreEqual("This is a test!", SimpleEncryption1AlternatingSplit.Decrypt("hsi  etTi sats!", 1));
            Assert.AreEqual("This is a test!", SimpleEncryption1AlternatingSplit.Decrypt("s eT ashi tist!", 2));
            Assert.AreEqual("This is a test!", SimpleEncryption1AlternatingSplit.Decrypt(" Tah itse sits!", 3));
            Assert.AreEqual("This is a test!", SimpleEncryption1AlternatingSplit.Decrypt("This is a test!", 4));
            Assert.AreEqual("This is a test!", SimpleEncryption1AlternatingSplit.Decrypt("This is a test!", -1));
        }

        [Test]
        public void EmptyTests()
        {
            Assert.AreEqual("", SimpleEncryption1AlternatingSplit.Encrypt("", 0));
            Assert.AreEqual("", SimpleEncryption1AlternatingSplit.Decrypt("", 0));
        }

        [Test]
        public void NullTests()
        {
            Assert.AreEqual(null, SimpleEncryption1AlternatingSplit.Encrypt(null, 0));
            Assert.AreEqual(null, SimpleEncryption1AlternatingSplit.Decrypt(null, 0));
        }
    }
}