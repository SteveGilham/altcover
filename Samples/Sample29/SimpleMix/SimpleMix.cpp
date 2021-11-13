// CLRCaller.cpp : main project file.

#include "stdafx.h"

#pragma unmanaged
int NativeCountVowels(wchar_t* pString)
{
  int count = 0;
  const wchar_t* vowels = L"aeiouAEIOU";
  while (*pString)
  {
    if (wcschr(vowels, *pString++))
      count++;
  }
  return count;
}
#pragma managed

using namespace System;

ref class Example {
public:
  static int main(array<System::String^>^ args)
  {
    String^ str = "Text to be hashed";
    Console::WriteLine(str);
    pin_ptr<Char> p =
      const_cast<interior_ptr<Char>> (
        PtrToStringChars(str));
    Console::WriteLine(NativeCountVowels(p));

    return 0;
  }
};

int main(array<System::String^>^ args)
{
  if (! Example::main(args))
  {
     String^ str = "Text to be hashed";
     Console::WriteLine(str);
    pin_ptr<Char> p =
      const_cast<interior_ptr<Char>> (
        PtrToStringChars(str));
    Console::WriteLine(NativeCountVowels(p));

    return 0;
  }
}