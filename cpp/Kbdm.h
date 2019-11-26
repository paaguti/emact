/*
 * static auto rcsid("$Id: ./emacs.h,v 1.66 2018/09/09 07:25:14 jullien Exp $");
 */

/*
 * This  program  is  free  software;  you can redistribute it and/or
 * modify  it  under  the  terms of the GNU General Public License as
 * published  by  the  Free  Software Foundation; either version 2 of
 * the License, or (at your option) any later version.
 *
 * This  program  is  distributed in the hope that it will be useful,
 * but  WITHOUT ANY WARRANTY;  without  even the implied  warranty of
 * MERCHANTABILITY  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You  should have received a copy of the GNU General Public License
 * along  with  this  program;  if  not,  write  to the Free Software
 * Foundation,  Inc.,  59  Temple  Place  -  Suite  330,  Boston,  MA
 * 02111-1307, USA.
 */

#if !defined(__KBDM_H)
#define __KBDM_H
class Kbdm {
 public:
  class BufferFullException {};
  Kbdm() {
    reset();
  }

  /*
   * Playing methods
   */
  void
  startPlaying() noexcept {
    _kbdmop = &_kbdm[0];
  }

  void
  stopPlaying() noexcept {
    _kbdmop = nullptr;
  }

  bool
  isPlaying() noexcept {
    return _kbdmop != nullptr;
  }

  int
  play() noexcept {
    return *_kbdmop++;
  }

  /*
   * Recording methods
   */
  void
  startRecording() noexcept {
    _kbdmip = &_kbdm[0];
  }

  void
  stopRecording() noexcept {
    _kbdmip = nullptr;
  }

  bool
  isRecording() const noexcept {
    return _kbdmip != nullptr;
  }

  void
  record(int c) {
    if (_kbdmip >= &_kbdm[NKBDM]) {
      reset();
      throw BufferFullException{};
    } else {
      *_kbdmip++ = c;
    }
  }

  void
  reset() noexcept {
    _kbdmip = nullptr;
    _kbdm[0] = -1;
  }

  bool
  exist() const noexcept {
    return _kbdm[0] != -1;
  }

 private:
  static constexpr size_t NKBDM = 512;  // # of strokes, keyboard macro
  int  _kbdm[NKBDM];                    // Holds keyboard macro data
  int* _kbdmip{nullptr};                // Input pointer for above
  int* _kbdmop{nullptr};                // Output pointer for above
};
#endif /* __KBDM_H */
