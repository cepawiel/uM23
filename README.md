# uM23 Cortex-M23 Emulator for VMU
uM23 is an ARM Cortex-M23 emulator written for the Sega Dreamcast VMU by [Dmitry Grinberg](https://dmitry.gr).
Using this as the GAME on a VMU will allow you to load ARM binary files and execute them. 

If you haven't already please go check out [Dmitry's blog](https://dmitry.gr) where there's loads of really amazing
projects including [Running Linux on an Atmel ATSAMD21](https://dmitry.gr/?r=05.Projects&proj=33.%20LinuxCard)

## Build Instructions
Make sure you have [Waterbear](https://github.com/wtetzner/waterbear) installed and then its as simple as:
```
git clone https://github.com/cepawiel/uM23
cd uM23
make all
```
The resulting VMS file will be located at build/uM23.vms

## Examples
TODO: Add Examples

## License
Please read the [License](LICENSE.txt) regarding commercial usage.
I (Colton) don't feel my changes are transformative enough to require licensing them separately, 
as long as you have a license from Dmitry to use uM23 commercially consider that enough to use my changes as well.

That said, I would love to see what you've created to run on uM23!

## Credits
| Name                                           | Note                                                                 |
|------------------------------------------------|------------------------------------------------------------------------------|
| [Dmitry Grinberg](https://dmitry.gr)           | Original Author of [uM23](https://dmitry.gr/?r=05.Projects&proj=25.%20VMU%20Hacking)|
| [Falco Girgis](https://github.com/gyrovorbis)  | Author of [ElysianVMU](http://evmu.elysianshadows.com/) VMU Emulator |
| [Walter Tetzner](https://github.com/wtetzner)  | Author of [Waterbear](https://github.com/wtetzner/waterbear) Assembler |
| [Marcus Comstedt](https://mc.pp.se/dc/)        | Useful Information Pages including [VMU](https://mc.pp.se/dc/vms/cpu.html), [VMS](https://mc.pp.se/dc/vms/fileheader.html), and [VMI](https://mc.pp.se/dc/vms/vmi.html) |
| [Dreamcast Wiki](https://dreamcast.wiki/VMU_development) | Loads of useful resources for VMU Programmers! |
