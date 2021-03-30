(module
  (type (;0;) (func (param i32 i32)))
  (type (;1;) (func (param i32 i32 i32) (result i32)))
  (type (;2;) (func (param i32 i32) (result i32)))
  (type (;3;) (func (param i32 i32 i32 i32)))
  (type (;4;) (func (result i32)))
  (type (;5;) (func (param i32 i32 i32)))
  (type (;6;) (func))
  (type (;7;) (func (param i32)))
  (type (;8;) (func (param i32 i32 i32 i32 i32)))
  (type (;9;) (func (param i32 i32 i32 i32 i32) (result i32)))
  (type (;10;) (func (param i32) (result i32)))
  (type (;11;) (func (param i64 i32) (result i32)))
  (type (;12;) (func (param i32) (result i64)))
  (type (;13;) (func (param i32 i32 i32 i32) (result i32)))
  (type (;14;) (func (param i32) (result i32)))
  (import "ethereum" "useGas" (func (;0;) (type 7)))
  (import "ethereum" "getCallDataSize" (func (;1;) (type 4)))
  (import "ethereum" "callDataCopy" (func (;2;) (type 5)))
  (import "ethereum" "finish" (func (;3;) (type 0)))
  (func (;4;) (type 6)
    (local i32)
    global.get 0
    i32.const 48
    i32.sub
    local.tee 0
    global.set 0
    call 1
    i32.const 31
    i32.add
    i32.const 5
    i32.shr_u
    i32.const 12
    i32.mul
    i32.const 60
    i32.add
    call 0
    local.get 0
    call 5
    local.get 0
    i32.const 40
    i32.add
    i64.const 0
    i64.store
    local.get 0
    i32.const 32
    i32.add
    i64.const 0
    i64.store
    local.get 0
    i32.const 24
    i32.add
    i64.const 0
    i64.store
    local.get 0
    i64.const 0
    i64.store offset=16
    local.get 0
    i32.load
    local.get 0
    i32.load offset=8
    local.get 0
    i32.const 16
    i32.add
    local.tee 0
    call 6
    local.get 0
    i32.const 32
    call 3
    unreachable)
  (func (;5;) (type 7) (param i32)
    (local i32 i32)
    block  ;; label = @1
      call 1
      local.tee 1
      i32.const -1
      i32.gt_s
      if  ;; label = @2
        block  ;; label = @3
          local.get 1
          i32.eqz
          if  ;; label = @4
            i32.const 1
            local.set 2
            br 1 (;@3;)
          end
          local.get 1
          call 23
          local.tee 2
          i32.eqz
          br_if 2 (;@1;)
        end
        local.get 2
        i32.const 0
        local.get 1
        call 2
        local.get 0
        local.get 1
        i32.store offset=8
        local.get 0
        local.get 1
        i32.store offset=4
        local.get 0
        local.get 2
        i32.store
        return
      end
      i32.const 1049288
      call 17
      unreachable
    end
    local.get 1
    i32.const 1
    i32.const 1051004
    i32.load
    local.tee 0
    i32.const 1
    local.get 0
    select
    call_indirect (type 0)
    unreachable)
  (func (;6;) (type 5) (param i32 i32 i32)
    (local i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    i32.const 480
    i32.sub
    local.tee 3
    global.set 0
    local.get 3
    i32.const 48
    i32.add
    call 40
    drop
    local.get 3
    i32.const 1
    i32.store8 offset=256
    i32.const 136
    local.set 5
    local.get 3
    i32.const 136
    i32.store offset=252
    local.get 1
    local.set 6
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          loop  ;; label = @4
            local.get 4
            local.get 1
            i32.gt_u
            br_if 1 (;@3;)
            local.get 1
            local.get 4
            i32.sub
            local.set 7
            local.get 0
            local.get 4
            i32.add
            local.set 8
            local.get 6
            local.get 5
            i32.ge_u
            if  ;; label = @5
              local.get 3
              i32.const 48
              i32.add
              local.tee 9
              local.get 8
              local.get 7
              local.get 5
              call 16
              local.get 6
              local.get 5
              i32.sub
              local.set 6
              local.get 4
              local.get 5
              i32.add
              local.set 4
              local.get 9
              call 18
              local.get 3
              i32.load offset=252
              local.set 5
              br 1 (;@4;)
            end
          end
          local.get 3
          i32.const 48
          i32.add
          local.tee 0
          local.get 8
          local.get 7
          local.get 6
          call 16
          local.get 3
          local.get 6
          i32.store offset=248
          local.get 3
          i32.const 264
          i32.add
          local.tee 1
          local.get 0
          i32.const 216
          call 41
          drop
          local.get 3
          i32.load offset=468
          local.set 4
          local.get 3
          i32.load8_u offset=472
          local.set 5
          local.get 3
          i32.const 40
          i32.add
          local.get 1
          i32.const 200
          local.get 3
          i32.load offset=464
          call 7
          local.get 3
          i32.const 32
          i32.add
          local.get 3
          i32.load offset=40
          local.get 3
          i32.load offset=44
          i32.const 1
          call 11
          local.get 3
          i32.load offset=36
          i32.eqz
          br_if 1 (;@2;)
          local.get 3
          i32.load offset=32
          local.tee 6
          local.get 6
          i32.load8_u
          local.get 5
          i32.xor
          i32.store8
          local.get 3
          i32.const 24
          i32.add
          local.get 3
          i32.const 264
          i32.add
          i32.const 200
          local.get 4
          i32.const -1
          i32.add
          call 7
          local.get 3
          i32.const 16
          i32.add
          local.get 3
          i32.load offset=24
          local.get 3
          i32.load offset=28
          i32.const 1
          call 11
          local.get 3
          i32.load offset=20
          i32.eqz
          br_if 2 (;@1;)
          local.get 3
          i32.load offset=16
          local.tee 4
          local.get 4
          i32.load8_u
          i32.const 128
          i32.xor
          i32.store8
          local.get 3
          i32.const 264
          i32.add
          call 18
          i32.const 0
          local.set 5
          local.get 3
          i32.load offset=468
          local.set 6
          i32.const 32
          local.set 4
          loop  ;; label = @4
            local.get 3
            i32.const 8
            i32.add
            local.get 2
            i32.const 32
            local.get 5
            call 7
            local.get 3
            i32.load offset=12
            local.set 1
            local.get 3
            i32.load offset=8
            local.set 7
            local.get 4
            local.get 6
            i32.ge_u
            if  ;; label = @5
              local.get 3
              i32.const 264
              i32.add
              local.tee 0
              local.get 7
              local.get 1
              local.get 3
              i32.load offset=468
              call 12
              local.get 0
              call 18
              local.get 4
              local.get 3
              i32.load offset=468
              local.tee 6
              i32.sub
              local.set 4
              local.get 5
              local.get 6
              i32.add
              local.set 5
              br 1 (;@4;)
            end
          end
          local.get 3
          i32.const 264
          i32.add
          local.get 7
          local.get 1
          local.get 4
          call 12
          local.get 3
          i32.const 480
          i32.add
          global.set 0
          return
        end
        local.get 4
        local.get 1
        call 10
        unreachable
      end
      i32.const 1048732
      i32.const 0
      i32.const 0
      call 19
      unreachable
    end
    i32.const 1048844
    i32.const 0
    i32.const 0
    call 19
    unreachable)
  (func (;7;) (type 3) (param i32 i32 i32 i32)
    (local i32)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 4
    global.set 0
    local.get 4
    i32.const 8
    i32.add
    local.get 3
    local.get 2
    local.get 1
    local.get 2
    call 8
    local.get 0
    local.get 4
    i64.load offset=8
    i64.store align=4
    local.get 4
    i32.const 16
    i32.add
    global.set 0)
  (func (;8;) (type 8) (param i32 i32 i32 i32 i32)
    block  ;; label = @1
      local.get 2
      local.get 1
      i32.ge_u
      if  ;; label = @2
        local.get 4
        local.get 2
        i32.ge_u
        br_if 1 (;@1;)
        local.get 2
        local.get 4
        call 9
        unreachable
      end
      local.get 1
      local.get 2
      call 10
      unreachable
    end
    local.get 0
    local.get 2
    local.get 1
    i32.sub
    i32.store offset=4
    local.get 0
    local.get 1
    local.get 3
    i32.add
    i32.store)
  (func (;9;) (type 0) (param i32 i32)
    (local i32)
    global.get 0
    i32.const 48
    i32.sub
    local.tee 2
    global.set 0
    local.get 2
    local.get 1
    i32.store offset=4
    local.get 2
    local.get 0
    i32.store
    local.get 2
    i32.const 28
    i32.add
    i32.const 2
    i32.store
    local.get 2
    i32.const 44
    i32.add
    i32.const 2
    i32.store
    local.get 2
    i64.const 2
    i64.store offset=12 align=4
    local.get 2
    i32.const 1049636
    i32.store offset=8
    local.get 2
    i32.const 2
    i32.store offset=36
    local.get 2
    local.get 2
    i32.const 32
    i32.add
    i32.store offset=24
    local.get 2
    local.get 2
    i32.const 4
    i32.add
    i32.store offset=40
    local.get 2
    local.get 2
    i32.store offset=32
    local.get 2
    i32.const 8
    i32.add
    i32.const 1049652
    call 15
    unreachable)
  (func (;10;) (type 0) (param i32 i32)
    (local i32)
    global.get 0
    i32.const 48
    i32.sub
    local.tee 2
    global.set 0
    local.get 2
    local.get 1
    i32.store offset=4
    local.get 2
    local.get 0
    i32.store
    local.get 2
    i32.const 28
    i32.add
    i32.const 2
    i32.store
    local.get 2
    i32.const 44
    i32.add
    i32.const 2
    i32.store
    local.get 2
    i64.const 2
    i64.store offset=12 align=4
    local.get 2
    i32.const 1049732
    i32.store offset=8
    local.get 2
    i32.const 2
    i32.store offset=36
    local.get 2
    local.get 2
    i32.const 32
    i32.add
    i32.store offset=24
    local.get 2
    local.get 2
    i32.const 4
    i32.add
    i32.store offset=40
    local.get 2
    local.get 2
    i32.store offset=32
    local.get 2
    i32.const 8
    i32.add
    i32.const 1049748
    call 15
    unreachable)
  (func (;11;) (type 3) (param i32 i32 i32 i32)
    (local i32)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 4
    global.set 0
    local.get 4
    i32.const 8
    i32.add
    i32.const 0
    local.get 3
    local.get 1
    local.get 2
    call 8
    local.get 0
    local.get 4
    i64.load offset=8
    i64.store align=4
    local.get 4
    i32.const 16
    i32.add
    global.set 0)
  (func (;12;) (type 3) (param i32 i32 i32 i32)
    (local i32 i32)
    global.get 0
    i32.const 112
    i32.sub
    local.tee 4
    global.set 0
    local.get 4
    i32.const 16
    i32.add
    local.get 0
    i32.const 200
    i32.const 0
    call 7
    local.get 4
    i32.const 8
    i32.add
    local.get 4
    i32.load offset=16
    local.get 4
    i32.load offset=20
    local.get 3
    call 11
    local.get 4
    i32.load offset=8
    local.set 5
    local.get 4
    i32.load offset=12
    local.set 0
    local.get 4
    local.get 1
    local.get 2
    local.get 3
    call 11
    local.get 4
    i32.load
    local.get 4
    local.get 4
    i32.load offset=4
    local.tee 1
    i32.store offset=24
    local.get 4
    local.get 0
    i32.store offset=28
    local.get 0
    local.get 1
    i32.ne
    if  ;; label = @1
      local.get 4
      i32.const 56
      i32.add
      local.tee 0
      i32.const 20
      i32.add
      i32.const 3
      i32.store
      local.get 4
      i32.const 68
      i32.add
      i32.const 4
      i32.store
      local.get 4
      i32.const 32
      i32.add
      local.tee 1
      i32.const 20
      i32.add
      i32.const 3
      i32.store
      local.get 4
      i64.const 3
      i64.store offset=36 align=4
      local.get 4
      i32.const 1049056
      i32.store offset=32
      local.get 4
      i32.const 4
      i32.store offset=60
      local.get 4
      local.get 4
      i32.const 24
      i32.add
      i32.store offset=80
      local.get 4
      local.get 4
      i32.const 28
      i32.add
      i32.store offset=84
      local.get 4
      i64.const 4
      i64.store offset=104
      local.get 4
      i64.const 1
      i64.store offset=92 align=4
      local.get 4
      i32.const 1049140
      i32.store offset=88
      local.get 4
      local.get 0
      i32.store offset=48
      local.get 4
      local.get 4
      i32.const 88
      i32.add
      i32.store offset=72
      local.get 4
      local.get 4
      i32.const 84
      i32.add
      i32.store offset=64
      local.get 4
      local.get 4
      i32.const 80
      i32.add
      i32.store offset=56
      local.get 1
      i32.const 1049200
      call 15
      unreachable
    end
    local.get 5
    local.get 0
    call 41
    drop
    local.get 4
    i32.const 112
    i32.add
    global.set 0)
  (func (;13;) (type 2) (param i32 i32) (result i32)
    (local i32 i32 i32)
    global.get 0
    i32.const 32
    i32.sub
    local.tee 2
    global.set 0
    local.get 1
    i32.const 28
    i32.add
    i32.load
    local.set 3
    local.get 1
    i32.load offset=24
    local.get 2
    i32.const 8
    i32.add
    local.tee 1
    i32.const 16
    i32.add
    local.get 0
    i32.const 16
    i32.add
    i64.load align=4
    i64.store
    local.get 1
    i32.const 8
    i32.add
    local.get 0
    i32.const 8
    i32.add
    i64.load align=4
    i64.store
    local.get 2
    local.get 0
    i64.load align=4
    i64.store offset=8
    local.get 3
    local.get 1
    call 39
    local.get 2
    i32.const 32
    i32.add
    global.set 0)
  (func (;14;) (type 2) (param i32 i32) (result i32)
    (local i32 i32 i32)
    global.get 0
    i32.const 128
    i32.sub
    local.tee 3
    global.set 0
    local.get 0
    i32.load
    local.set 0
    block  ;; label = @1
      block  ;; label = @2
        block (result i32)  ;; label = @3
          block  ;; label = @4
            local.get 1
            i32.load
            local.tee 2
            i32.const 16
            i32.and
            i32.eqz
            if  ;; label = @5
              local.get 2
              i32.const 32
              i32.and
              br_if 1 (;@4;)
              local.get 0
              local.get 1
              call 20
              br 2 (;@3;)
            end
            local.get 0
            i32.load
            local.set 2
            i32.const 0
            local.set 0
            loop  ;; label = @5
              local.get 0
              local.get 3
              i32.add
              i32.const 127
              i32.add
              local.get 2
              i32.const 15
              i32.and
              local.tee 4
              i32.const 48
              i32.or
              local.get 4
              i32.const 87
              i32.add
              local.get 4
              i32.const 10
              i32.lt_u
              select
              i32.store8
              local.get 0
              i32.const -1
              i32.add
              local.set 0
              local.get 2
              i32.const 4
              i32.shr_u
              local.tee 2
              br_if 0 (;@5;)
            end
            local.get 0
            i32.const 128
            i32.add
            local.tee 2
            i32.const 129
            i32.ge_u
            br_if 2 (;@2;)
            local.get 1
            i32.const 1049799
            i32.const 2
            local.get 0
            local.get 3
            i32.add
            i32.const 128
            i32.add
            i32.const 0
            local.get 0
            i32.sub
            call 21
            br 1 (;@3;)
          end
          local.get 0
          i32.load
          local.set 2
          i32.const 0
          local.set 0
          loop  ;; label = @4
            local.get 0
            local.get 3
            i32.add
            i32.const 127
            i32.add
            local.get 2
            i32.const 15
            i32.and
            local.tee 4
            i32.const 48
            i32.or
            local.get 4
            i32.const 55
            i32.add
            local.get 4
            i32.const 10
            i32.lt_u
            select
            i32.store8
            local.get 0
            i32.const -1
            i32.add
            local.set 0
            local.get 2
            i32.const 4
            i32.shr_u
            local.tee 2
            br_if 0 (;@4;)
          end
          local.get 0
          i32.const 128
          i32.add
          local.tee 2
          i32.const 129
          i32.ge_u
          br_if 2 (;@1;)
          local.get 1
          i32.const 1049799
          i32.const 2
          local.get 0
          local.get 3
          i32.add
          i32.const 128
          i32.add
          i32.const 0
          local.get 0
          i32.sub
          call 21
        end
        local.get 3
        i32.const 128
        i32.add
        global.set 0
        return
      end
      local.get 2
      i32.const 128
      call 10
      unreachable
    end
    local.get 2
    i32.const 128
    call 10
    unreachable)
  (func (;15;) (type 0) (param i32 i32)
    (local i32 i64)
    global.get 0
    i32.const 32
    i32.sub
    local.tee 2
    global.set 0
    local.get 1
    i64.load align=4
    local.set 3
    local.get 2
    i32.const 20
    i32.add
    local.get 1
    i64.load offset=8 align=4
    i64.store align=4
    local.get 2
    local.get 3
    i64.store offset=12 align=4
    local.get 2
    local.get 0
    i32.store offset=8
    local.get 2
    i32.const 1049368
    i32.store offset=4
    local.get 2
    i32.const 1
    i32.store
    local.get 2
    call 35
    unreachable)
  (func (;16;) (type 3) (param i32 i32 i32 i32)
    (local i32)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 4
    global.set 0
    local.get 4
    i32.const 8
    i32.add
    local.get 0
    i32.const 200
    i32.const 0
    call 7
    local.get 4
    local.get 4
    i32.load offset=8
    local.get 4
    i32.load offset=12
    local.get 3
    call 11
    local.get 4
    i32.load offset=4
    local.tee 3
    local.get 2
    i32.le_u
    if  ;; label = @1
      local.get 4
      i32.load
      local.set 2
      loop  ;; label = @2
        local.get 3
        if  ;; label = @3
          local.get 2
          local.get 2
          i32.load8_u
          local.get 1
          i32.load8_u
          i32.xor
          i32.store8
          local.get 3
          i32.const -1
          i32.add
          local.set 3
          local.get 2
          i32.const 1
          i32.add
          local.set 2
          local.get 1
          i32.const 1
          i32.add
          local.set 1
          br 1 (;@2;)
        end
      end
      local.get 4
      i32.const 16
      i32.add
      global.set 0
      return
    end
    i32.const 1048576
    call 17
    unreachable)
  (func (;17;) (type 7) (param i32)
    (local i32 i64 i64 i64)
    global.get 0
    i32.const 48
    i32.sub
    local.tee 1
    global.set 0
    local.get 0
    i64.load offset=8 align=4
    local.set 2
    local.get 0
    i64.load offset=16 align=4
    local.set 3
    local.get 0
    i64.load align=4
    local.set 4
    local.get 1
    i64.const 4
    i64.store offset=16
    local.get 1
    i64.const 1
    i64.store offset=4 align=4
    local.get 1
    local.get 4
    i64.store offset=24
    local.get 1
    local.get 1
    i32.const 24
    i32.add
    i32.store
    local.get 1
    local.get 3
    i64.store offset=40
    local.get 1
    local.get 2
    i64.store offset=32
    local.get 1
    local.get 1
    i32.const 32
    i32.add
    call 15
    unreachable)
  (func (;18;) (type 7) (param i32)
    (local i32 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64 i64)
    local.get 0
    i64.load offset=192
    local.set 12
    local.get 0
    i64.load offset=152
    local.set 4
    local.get 0
    i64.load offset=112
    local.set 13
    local.get 0
    i64.load offset=72
    local.set 14
    local.get 0
    i64.load offset=32
    local.set 8
    local.get 0
    i64.load offset=184
    local.set 9
    local.get 0
    i64.load offset=144
    local.set 15
    local.get 0
    i64.load offset=104
    local.set 16
    local.get 0
    i64.load offset=64
    local.set 11
    local.get 0
    i64.load offset=24
    local.set 17
    local.get 0
    i64.load offset=176
    local.set 18
    local.get 0
    i64.load offset=136
    local.set 19
    local.get 0
    i64.load offset=96
    local.set 20
    local.get 0
    i64.load offset=56
    local.set 21
    local.get 0
    i64.load offset=16
    local.set 5
    local.get 0
    i64.load offset=168
    local.set 22
    local.get 0
    i64.load offset=128
    local.set 23
    local.get 0
    i64.load offset=88
    local.set 24
    local.get 0
    i64.load offset=48
    local.set 25
    local.get 0
    i64.load offset=8
    local.set 26
    local.get 0
    i64.load offset=160
    local.set 10
    local.get 0
    i64.load offset=120
    local.set 27
    local.get 0
    i64.load offset=80
    local.set 28
    local.get 0
    i64.load offset=40
    local.set 29
    local.get 0
    i64.load
    local.set 30
    i32.const -192
    local.set 1
    loop  ;; label = @1
      local.get 1
      if  ;; label = @2
        local.get 25
        local.get 29
        local.get 30
        i64.xor
        local.get 28
        i64.xor
        local.get 27
        i64.xor
        local.get 10
        i64.xor
        local.tee 3
        local.get 5
        local.get 21
        i64.xor
        local.get 20
        i64.xor
        local.get 19
        i64.xor
        local.get 18
        i64.xor
        local.tee 6
        i64.const 1
        i64.rotl
        i64.xor
        local.tee 2
        i64.xor
        local.set 45
        local.get 12
        local.get 11
        local.get 17
        i64.xor
        local.get 16
        i64.xor
        local.get 15
        i64.xor
        local.get 9
        i64.xor
        local.tee 7
        local.get 3
        i64.const 1
        i64.rotl
        i64.xor
        local.tee 3
        i64.xor
        local.set 46
        local.get 2
        local.get 22
        i64.xor
        i64.const 2
        i64.rotl
        local.tee 32
        local.get 11
        local.get 8
        local.get 14
        i64.xor
        local.get 13
        i64.xor
        local.get 4
        i64.xor
        local.get 12
        i64.xor
        local.tee 31
        i64.const 1
        i64.rotl
        local.get 6
        i64.xor
        local.tee 6
        i64.xor
        i64.const 55
        i64.rotl
        local.tee 33
        local.get 25
        local.get 26
        i64.xor
        local.get 24
        i64.xor
        local.get 23
        i64.xor
        local.get 22
        i64.xor
        local.tee 11
        local.get 7
        i64.const 1
        i64.rotl
        i64.xor
        local.tee 7
        local.get 5
        i64.xor
        i64.const 62
        i64.rotl
        local.tee 34
        i64.const -1
        i64.xor
        i64.and
        i64.xor
        local.set 12
        local.get 27
        local.get 11
        i64.const 1
        i64.rotl
        local.get 31
        i64.xor
        local.tee 5
        i64.xor
        i64.const 41
        i64.rotl
        local.tee 31
        local.get 3
        local.get 13
        i64.xor
        i64.const 39
        i64.rotl
        local.tee 35
        i64.const -1
        i64.xor
        i64.and
        local.get 33
        i64.xor
        local.set 22
        local.get 2
        local.get 24
        i64.xor
        i64.const 10
        i64.rotl
        local.tee 36
        local.get 6
        local.get 9
        i64.xor
        i64.const 56
        i64.rotl
        local.tee 37
        local.get 7
        local.get 19
        i64.xor
        i64.const 15
        i64.rotl
        local.tee 38
        i64.const -1
        i64.xor
        i64.and
        i64.xor
        local.set 19
        local.get 3
        local.get 8
        i64.xor
        i64.const 27
        i64.rotl
        local.tee 39
        local.get 5
        local.get 29
        i64.xor
        i64.const 36
        i64.rotl
        local.tee 40
        i64.const -1
        i64.xor
        local.get 36
        i64.and
        i64.xor
        local.set 27
        local.get 5
        local.get 10
        i64.xor
        i64.const 18
        i64.rotl
        local.tee 10
        local.get 7
        local.get 21
        i64.xor
        i64.const 6
        i64.rotl
        local.tee 41
        local.get 2
        local.get 26
        i64.xor
        i64.const 1
        i64.rotl
        local.tee 42
        i64.const -1
        i64.xor
        i64.and
        i64.xor
        local.set 13
        local.get 3
        local.get 4
        i64.xor
        i64.const 8
        i64.rotl
        local.tee 43
        local.get 6
        local.get 16
        i64.xor
        i64.const 25
        i64.rotl
        local.tee 44
        i64.const -1
        i64.xor
        i64.and
        local.get 41
        i64.xor
        local.set 24
        local.get 7
        local.get 18
        i64.xor
        i64.const 61
        i64.rotl
        local.tee 8
        local.get 3
        local.get 14
        i64.xor
        i64.const 20
        i64.rotl
        local.tee 3
        local.get 6
        local.get 17
        i64.xor
        i64.const 28
        i64.rotl
        local.tee 4
        i64.const -1
        i64.xor
        i64.and
        i64.xor
        local.set 14
        local.get 2
        local.get 23
        i64.xor
        i64.const 45
        i64.rotl
        local.tee 2
        local.get 8
        i64.const -1
        i64.xor
        local.get 4
        i64.and
        i64.xor
        local.set 11
        local.get 5
        local.get 28
        i64.xor
        i64.const 3
        i64.rotl
        local.tee 9
        local.get 2
        i64.const -1
        i64.xor
        local.get 8
        i64.and
        i64.xor
        local.set 21
        local.get 9
        i64.const -1
        i64.xor
        local.get 2
        i64.and
        local.get 3
        i64.xor
        local.set 25
        local.get 3
        i64.const -1
        i64.xor
        local.get 9
        i64.and
        local.get 4
        i64.xor
        local.set 29
        local.get 6
        local.get 15
        i64.xor
        i64.const 21
        i64.rotl
        local.tee 2
        local.get 5
        local.get 30
        i64.xor
        local.tee 3
        local.get 46
        i64.const 14
        i64.rotl
        local.tee 6
        i64.const -1
        i64.xor
        i64.and
        i64.xor
        local.set 17
        local.get 7
        local.get 20
        i64.xor
        i64.const 43
        i64.rotl
        local.tee 7
        local.get 2
        i64.const -1
        i64.xor
        local.get 6
        i64.and
        i64.xor
        local.set 5
        local.get 7
        i64.const -1
        i64.xor
        local.get 2
        i64.and
        local.get 45
        i64.const 44
        i64.rotl
        local.tee 2
        i64.xor
        local.set 26
        local.get 1
        i32.const 1049056
        i32.add
        i64.load
        local.get 2
        i64.const -1
        i64.xor
        local.get 7
        i64.and
        local.get 3
        i64.xor
        i64.xor
        local.set 30
        local.get 1
        i32.const 8
        i32.add
        local.set 1
        local.get 39
        i64.const -1
        i64.xor
        local.get 40
        i64.and
        local.get 37
        i64.xor
        local.set 4
        local.get 3
        i64.const -1
        i64.xor
        local.get 2
        i64.and
        local.get 6
        i64.xor
        local.set 8
        local.get 32
        i64.const -1
        i64.xor
        local.get 34
        i64.and
        local.get 31
        i64.xor
        local.set 9
        local.get 37
        i64.const -1
        i64.xor
        local.get 39
        i64.and
        local.get 38
        i64.xor
        local.set 15
        local.get 10
        i64.const -1
        i64.xor
        local.get 42
        i64.and
        local.get 43
        i64.xor
        local.set 16
        local.get 31
        i64.const -1
        i64.xor
        local.get 32
        i64.and
        local.get 35
        i64.xor
        local.set 18
        local.get 43
        i64.const -1
        i64.xor
        local.get 10
        i64.and
        local.get 44
        i64.xor
        local.set 20
        local.get 36
        i64.const -1
        i64.xor
        local.get 38
        i64.and
        local.get 40
        i64.xor
        local.set 23
        local.get 33
        i64.const -1
        i64.xor
        local.get 35
        i64.and
        local.get 34
        i64.xor
        local.set 10
        local.get 41
        i64.const -1
        i64.xor
        local.get 44
        i64.and
        local.get 42
        i64.xor
        local.set 28
        br 1 (;@1;)
      else
        local.get 0
        local.get 10
        i64.store offset=160
        local.get 0
        local.get 27
        i64.store offset=120
        local.get 0
        local.get 28
        i64.store offset=80
        local.get 0
        local.get 29
        i64.store offset=40
        local.get 0
        local.get 30
        i64.store
        local.get 0
        local.get 22
        i64.store offset=168
        local.get 0
        local.get 23
        i64.store offset=128
        local.get 0
        local.get 24
        i64.store offset=88
        local.get 0
        local.get 25
        i64.store offset=48
        local.get 0
        local.get 26
        i64.store offset=8
        local.get 0
        local.get 18
        i64.store offset=176
        local.get 0
        local.get 19
        i64.store offset=136
        local.get 0
        local.get 20
        i64.store offset=96
        local.get 0
        local.get 21
        i64.store offset=56
        local.get 0
        local.get 5
        i64.store offset=16
        local.get 0
        local.get 9
        i64.store offset=184
        local.get 0
        local.get 15
        i64.store offset=144
        local.get 0
        local.get 16
        i64.store offset=104
        local.get 0
        local.get 11
        i64.store offset=64
        local.get 0
        local.get 17
        i64.store offset=24
        local.get 0
        local.get 12
        i64.store offset=192
        local.get 0
        local.get 4
        i64.store offset=152
        local.get 0
        local.get 13
        i64.store offset=112
        local.get 0
        local.get 14
        i64.store offset=72
        local.get 0
        local.get 8
        i64.store offset=32
      end
    end)
  (func (;19;) (type 5) (param i32 i32 i32)
    (local i32)
    global.get 0
    i32.const 48
    i32.sub
    local.tee 3
    global.set 0
    local.get 3
    local.get 2
    i32.store offset=4
    local.get 3
    local.get 1
    i32.store
    local.get 3
    i32.const 28
    i32.add
    i32.const 2
    i32.store
    local.get 3
    i32.const 44
    i32.add
    i32.const 2
    i32.store
    local.get 3
    i64.const 2
    i64.store offset=12 align=4
    local.get 3
    i32.const 1049352
    i32.store offset=8
    local.get 3
    i32.const 2
    i32.store offset=36
    local.get 3
    local.get 3
    i32.const 32
    i32.add
    i32.store offset=24
    local.get 3
    local.get 3
    i32.store offset=40
    local.get 3
    local.get 3
    i32.const 4
    i32.add
    i32.store offset=32
    local.get 3
    i32.const 8
    i32.add
    local.get 0
    call 15
    unreachable)
  (func (;20;) (type 2) (param i32 i32) (result i32)
    local.get 0
    i64.load32_u
    local.get 1
    call 36)
  (func (;21;) (type 9) (param i32 i32 i32 i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32)
    local.get 0
    i32.load
    local.tee 9
    i32.const 1
    i32.and
    local.tee 10
    local.get 4
    i32.add
    local.set 8
    block  ;; label = @1
      local.get 9
      i32.const 4
      i32.and
      i32.eqz
      if  ;; label = @2
        i32.const 0
        local.set 1
        br 1 (;@1;)
      end
      local.get 2
      if  ;; label = @2
        local.get 2
        local.set 6
        local.get 1
        local.set 7
        loop  ;; label = @3
          local.get 7
          i32.load8_u
          i32.const 192
          i32.and
          i32.const 128
          i32.eq
          local.get 5
          i32.add
          local.set 5
          local.get 7
          i32.const 1
          i32.add
          local.set 7
          local.get 6
          i32.const -1
          i32.add
          local.tee 6
          br_if 0 (;@3;)
        end
      end
      local.get 2
      local.get 8
      i32.add
      local.get 5
      i32.sub
      local.set 8
    end
    i32.const 43
    i32.const 1114112
    local.get 10
    select
    local.set 5
    block  ;; label = @1
      block  ;; label = @2
        local.get 0
        i32.load offset=8
        i32.const 1
        i32.ne
        if  ;; label = @3
          local.get 0
          local.get 5
          local.get 1
          local.get 2
          call 38
          br_if 1 (;@2;)
          br 2 (;@1;)
        end
        local.get 0
        i32.const 12
        i32.add
        i32.load
        local.tee 6
        local.get 8
        i32.le_u
        if  ;; label = @3
          local.get 0
          local.get 5
          local.get 1
          local.get 2
          call 38
          br_if 1 (;@2;)
          br 2 (;@1;)
        end
        block  ;; label = @3
          local.get 9
          i32.const 8
          i32.and
          i32.eqz
          if  ;; label = @4
            local.get 6
            local.get 8
            i32.sub
            local.set 6
            i32.const 0
            local.set 7
            block  ;; label = @5
              block  ;; label = @6
                block  ;; label = @7
                  i32.const 1
                  local.get 0
                  i32.load8_u offset=48
                  local.tee 8
                  local.get 8
                  i32.const 3
                  i32.eq
                  select
                  i32.const 1
                  i32.sub
                  br_table 0 (;@7;) 1 (;@6;) 0 (;@7;) 2 (;@5;)
                end
                local.get 6
                local.set 7
                i32.const 0
                local.set 6
                br 1 (;@5;)
              end
              local.get 6
              i32.const 1
              i32.shr_u
              local.set 7
              local.get 6
              i32.const 1
              i32.add
              i32.const 1
              i32.shr_u
              local.set 6
            end
            local.get 7
            i32.const 1
            i32.add
            local.set 7
            loop  ;; label = @5
              local.get 7
              i32.const -1
              i32.add
              local.tee 7
              i32.eqz
              br_if 2 (;@3;)
              local.get 0
              i32.load offset=24
              local.get 0
              i32.load offset=4
              local.get 0
              i32.load offset=28
              i32.load offset=16
              call_indirect (type 2)
              i32.eqz
              br_if 0 (;@5;)
            end
            i32.const 1
            return
          end
          local.get 0
          i32.const 1
          i32.store8 offset=48
          local.get 0
          i32.const 48
          i32.store offset=4
          local.get 0
          local.get 5
          local.get 1
          local.get 2
          call 38
          br_if 1 (;@2;)
          local.get 6
          local.get 8
          i32.sub
          local.set 5
          i32.const 0
          local.set 7
          block  ;; label = @4
            block  ;; label = @5
              block  ;; label = @6
                i32.const 1
                local.get 0
                i32.load8_u offset=48
                local.tee 6
                local.get 6
                i32.const 3
                i32.eq
                select
                i32.const 1
                i32.sub
                br_table 0 (;@6;) 1 (;@5;) 0 (;@6;) 2 (;@4;)
              end
              local.get 5
              local.set 7
              i32.const 0
              local.set 5
              br 1 (;@4;)
            end
            local.get 5
            i32.const 1
            i32.shr_u
            local.set 7
            local.get 5
            i32.const 1
            i32.add
            i32.const 1
            i32.shr_u
            local.set 5
          end
          local.get 7
          i32.const 1
          i32.add
          local.set 7
          block  ;; label = @4
            loop  ;; label = @5
              local.get 7
              i32.const -1
              i32.add
              local.tee 7
              i32.eqz
              br_if 1 (;@4;)
              local.get 0
              i32.load offset=24
              local.get 0
              i32.load offset=4
              local.get 0
              i32.load offset=28
              i32.load offset=16
              call_indirect (type 2)
              i32.eqz
              br_if 0 (;@5;)
            end
            i32.const 1
            return
          end
          local.get 0
          i32.load offset=4
          local.set 6
          local.get 0
          i32.load offset=24
          local.get 3
          local.get 4
          local.get 0
          i32.load offset=28
          i32.load offset=12
          call_indirect (type 1)
          br_if 1 (;@2;)
          local.get 5
          i32.const 1
          i32.add
          local.set 5
          local.get 0
          i32.load offset=28
          local.set 2
          local.get 0
          i32.load offset=24
          local.set 0
          loop  ;; label = @4
            local.get 5
            i32.const -1
            i32.add
            local.tee 5
            i32.eqz
            if  ;; label = @5
              i32.const 0
              return
            end
            local.get 0
            local.get 6
            local.get 2
            i32.load offset=16
            call_indirect (type 2)
            i32.eqz
            br_if 0 (;@4;)
          end
          br 1 (;@2;)
        end
        local.get 0
        i32.load offset=4
        local.set 8
        local.get 0
        local.get 5
        local.get 1
        local.get 2
        call 38
        br_if 0 (;@2;)
        local.get 0
        i32.load offset=24
        local.get 3
        local.get 4
        local.get 0
        i32.load offset=28
        i32.load offset=12
        call_indirect (type 1)
        br_if 0 (;@2;)
        local.get 6
        i32.const 1
        i32.add
        local.set 5
        local.get 0
        i32.load offset=28
        local.set 6
        local.get 0
        i32.load offset=24
        local.set 0
        loop  ;; label = @3
          local.get 5
          i32.const -1
          i32.add
          local.tee 5
          i32.eqz
          if  ;; label = @4
            i32.const 0
            return
          end
          local.get 0
          local.get 8
          local.get 6
          i32.load offset=16
          call_indirect (type 2)
          i32.eqz
          br_if 0 (;@3;)
        end
      end
      i32.const 1
      return
    end
    local.get 0
    i32.load offset=24
    local.get 3
    local.get 4
    local.get 0
    i32.const 28
    i32.add
    i32.load
    i32.load offset=12
    call_indirect (type 1))
  (func (;22;) (type 0) (param i32 i32)
    nop)
  (func (;23;) (type 14) (param i32) (result i32)
    (local i32 i32 i32)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 1
    global.set 0
    i32.const 1
    local.set 2
    block  ;; label = @1
      local.get 0
      i32.eqz
      br_if 0 (;@1;)
      local.get 0
      i32.const 3
      i32.add
      i32.const 2
      i32.shr_u
      local.tee 0
      i32.const -1
      i32.add
      local.tee 3
      i32.const 255
      i32.le_u
      if  ;; label = @2
        local.get 1
        i32.const 1049960
        i32.store offset=4
        local.get 1
        local.get 3
        i32.const 2
        i32.shl
        i32.const 1049964
        i32.add
        local.tee 3
        i32.load
        i32.store offset=12
        local.get 0
        i32.const 1
        local.get 1
        i32.const 12
        i32.add
        local.get 1
        i32.const 4
        i32.add
        i32.const 1049240
        call 25
        local.set 2
        local.get 3
        local.get 1
        i32.load offset=12
        i32.store
        br 1 (;@1;)
      end
      local.get 1
      i32.const 1049960
      i32.load
      i32.store offset=8
      local.get 0
      i32.const 1
      local.get 1
      i32.const 8
      i32.add
      i32.const 1049634
      i32.const 1049264
      call 25
      local.set 2
      i32.const 1049960
      local.get 1
      i32.load offset=8
      i32.store
    end
    local.get 1
    i32.const 16
    i32.add
    global.set 0
    local.get 2)
  (func (;24;) (type 3) (param i32 i32 i32 i32)
    (local i32 i32)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 3
    global.set 0
    local.get 3
    local.get 1
    i32.load
    local.tee 4
    i32.load
    i32.store offset=12
    i32.const 1
    local.set 1
    local.get 2
    i32.const 2
    i32.add
    local.tee 2
    local.get 2
    i32.mul
    local.tee 2
    i32.const 2048
    local.get 2
    i32.const 2048
    i32.gt_u
    select
    local.tee 5
    i32.const 4
    local.get 3
    i32.const 12
    i32.add
    i32.const 1
    i32.const 1049216
    call 25
    local.set 2
    local.get 4
    local.get 3
    i32.load offset=12
    i32.store
    local.get 2
    if  ;; label = @1
      local.get 2
      i64.const 0
      i64.store offset=4 align=4
      local.get 2
      local.get 5
      i32.const 2
      i32.shl
      local.get 2
      i32.add
      i32.const 2
      i32.or
      i32.store
      i32.const 0
      local.set 1
    end
    local.get 0
    local.get 2
    i32.store offset=4
    local.get 0
    local.get 1
    i32.store
    local.get 3
    i32.const 16
    i32.add
    global.set 0)
  (func (;25;) (type 9) (param i32 i32 i32 i32 i32) (result i32)
    (local i32 i32)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 6
    global.set 0
    block  ;; label = @1
      local.get 0
      local.get 1
      local.get 2
      local.get 3
      local.get 4
      call 32
      local.tee 5
      br_if 0 (;@1;)
      local.get 6
      i32.const 8
      i32.add
      local.get 3
      local.get 0
      local.get 1
      local.get 4
      i32.load offset=12
      call_indirect (type 3)
      i32.const 0
      local.set 5
      local.get 6
      i32.load offset=8
      br_if 0 (;@1;)
      local.get 6
      i32.load offset=12
      local.tee 5
      local.get 2
      i32.load
      i32.store offset=8
      local.get 2
      local.get 5
      i32.store
      local.get 0
      local.get 1
      local.get 2
      local.get 3
      local.get 4
      call 32
      local.set 5
    end
    local.get 6
    i32.const 16
    i32.add
    global.set 0
    local.get 5)
  (func (;26;) (type 7) (param i32)
    nop)
  (func (;27;) (type 2) (param i32 i32) (result i32)
    local.get 1)
  (func (;28;) (type 10) (param i32) (result i32)
    i32.const 0)
  (func (;29;) (type 3) (param i32 i32 i32 i32)
    block (result i32)  ;; label = @1
      local.get 2
      i32.const 2
      i32.shl
      local.tee 2
      local.get 3
      i32.const 3
      i32.shl
      i32.const 16384
      i32.add
      local.tee 3
      local.get 3
      local.get 2
      i32.lt_u
      select
      i32.const 65543
      i32.add
      local.tee 1
      i32.const 16
      i32.shr_u
      memory.grow
      local.tee 3
      i32.const -1
      i32.eq
      if  ;; label = @2
        i32.const 0
        local.set 3
        i32.const 1
        br 1 (;@1;)
      end
      local.get 3
      i32.const 16
      i32.shl
      local.tee 3
      i64.const 0
      i64.store
      local.get 3
      i32.const 0
      i32.store offset=8
      local.get 3
      local.get 1
      i32.const -65536
      i32.and
      local.get 3
      i32.add
      i32.const 2
      i32.or
      i32.store
      i32.const 0
    end
    local.set 2
    local.get 0
    local.get 3
    i32.store offset=4
    local.get 0
    local.get 2
    i32.store)
  (func (;30;) (type 2) (param i32 i32) (result i32)
    i32.const 512)
  (func (;31;) (type 10) (param i32) (result i32)
    i32.const 1)
  (func (;32;) (type 9) (param i32 i32 i32 i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32)
    local.get 1
    i32.const -1
    i32.add
    local.set 8
    local.get 0
    i32.const 2
    i32.shl
    local.set 7
    i32.const 0
    local.get 1
    i32.sub
    local.set 9
    local.get 2
    i32.load
    local.set 5
    loop  ;; label = @1
      block  ;; label = @2
        local.get 5
        i32.eqz
        br_if 0 (;@2;)
        local.get 5
        local.set 1
        loop  ;; label = @3
          block  ;; label = @4
            local.get 1
            i32.load offset=8
            local.tee 5
            i32.const 1
            i32.and
            i32.eqz
            if  ;; label = @5
              local.get 1
              i32.load
              i32.const -4
              i32.and
              local.tee 10
              local.get 1
              i32.const 8
              i32.add
              local.tee 6
              i32.sub
              local.get 7
              i32.lt_u
              br_if 1 (;@4;)
              block  ;; label = @6
                local.get 3
                local.get 0
                local.get 4
                i32.load offset=16
                call_indirect (type 2)
                i32.const 2
                i32.shl
                local.get 6
                i32.add
                i32.const 8
                i32.add
                local.get 10
                local.get 7
                i32.sub
                local.get 9
                i32.and
                local.tee 5
                i32.gt_u
                if  ;; label = @7
                  local.get 6
                  i32.load
                  local.set 5
                  local.get 6
                  local.get 8
                  i32.and
                  br_if 3 (;@4;)
                  local.get 2
                  local.get 5
                  i32.const -4
                  i32.and
                  i32.store
                  local.get 1
                  local.set 5
                  br 1 (;@6;)
                end
                local.get 5
                i32.const 0
                i32.store
                local.get 5
                i32.const -8
                i32.add
                local.tee 5
                i64.const 0
                i64.store align=4
                local.get 5
                local.get 1
                i32.load
                i32.const -4
                i32.and
                i32.store
                block  ;; label = @7
                  local.get 1
                  i32.load
                  local.tee 6
                  i32.const -4
                  i32.and
                  local.tee 2
                  i32.eqz
                  br_if 0 (;@7;)
                  local.get 6
                  i32.const 2
                  i32.and
                  br_if 0 (;@7;)
                  local.get 2
                  local.get 2
                  i32.load offset=4
                  i32.const 3
                  i32.and
                  local.get 5
                  i32.or
                  i32.store offset=4
                end
                local.get 5
                local.get 5
                i32.load offset=4
                i32.const 3
                i32.and
                local.get 1
                i32.or
                i32.store offset=4
                local.get 1
                local.get 1
                i32.load offset=8
                i32.const -2
                i32.and
                i32.store offset=8
                local.get 1
                local.get 1
                i32.load
                local.tee 2
                i32.const 3
                i32.and
                local.get 5
                i32.or
                local.tee 6
                i32.store
                local.get 2
                i32.const 2
                i32.and
                i32.eqz
                br_if 0 (;@6;)
                local.get 1
                local.get 6
                i32.const -3
                i32.and
                i32.store
                local.get 5
                local.get 5
                i32.load
                i32.const 2
                i32.or
                i32.store
              end
              local.get 5
              local.get 5
              i32.load
              i32.const 1
              i32.or
              i32.store
              local.get 5
              i32.const 8
              i32.add
              local.set 11
              br 3 (;@2;)
            end
            local.get 1
            local.get 5
            i32.const -2
            i32.and
            i32.store offset=8
            block (result i32)  ;; label = @5
              i32.const 0
              local.get 1
              i32.load offset=4
              i32.const -4
              i32.and
              local.tee 5
              i32.eqz
              br_if 0 (;@5;)
              drop
              i32.const 0
              local.get 5
              local.get 5
              i32.load8_u
              i32.const 1
              i32.and
              select
            end
            local.set 5
            local.get 1
            call 33
            local.get 1
            i32.load8_u
            i32.const 2
            i32.and
            if  ;; label = @5
              local.get 5
              local.get 5
              i32.load
              i32.const 2
              i32.or
              i32.store
            end
            local.get 2
            local.get 5
            i32.store
            local.get 5
            local.set 1
            br 1 (;@3;)
          end
        end
        local.get 2
        local.get 5
        i32.store
        br 1 (;@1;)
      end
    end
    local.get 11)
  (func (;33;) (type 7) (param i32)
    (local i32 i32)
    block  ;; label = @1
      local.get 0
      i32.load
      local.tee 2
      i32.const -4
      i32.and
      local.tee 1
      i32.eqz
      br_if 0 (;@1;)
      local.get 2
      i32.const 2
      i32.and
      br_if 0 (;@1;)
      local.get 1
      local.get 1
      i32.load offset=4
      i32.const 3
      i32.and
      local.get 0
      i32.load offset=4
      i32.const -4
      i32.and
      i32.or
      i32.store offset=4
    end
    local.get 0
    i32.load offset=4
    local.tee 1
    i32.const -4
    i32.and
    local.tee 2
    if  ;; label = @1
      local.get 2
      local.get 2
      i32.load
      i32.const 3
      i32.and
      local.get 0
      i32.load
      i32.const -4
      i32.and
      i32.or
      i32.store
      local.get 0
      i32.load offset=4
      local.set 1
    end
    local.get 0
    local.get 1
    i32.const 3
    i32.and
    i32.store offset=4
    local.get 0
    local.get 0
    i32.load
    i32.const 3
    i32.and
    i32.store)
  (func (;34;) (type 6)
    (local i32 i32)
    i32.const 1
    local.set 0
    block  ;; label = @1
      block  ;; label = @2
        i32.const 1050992
        i32.load
        i32.const 1
        i32.ne
        if  ;; label = @3
          i32.const 1050992
          i64.const 4294967297
          i64.store
          br 1 (;@2;)
        end
        i32.const 1050996
        i32.const 1050996
        i32.load
        i32.const 1
        i32.add
        local.tee 0
        i32.store
        local.get 0
        i32.const 2
        i32.gt_u
        br_if 1 (;@1;)
      end
      i32.const 1051000
      i32.load
      local.tee 1
      i32.const -1
      i32.le_s
      br_if 0 (;@1;)
      i32.const 1051000
      local.get 1
      i32.store
      local.get 0
      i32.const 1
      i32.gt_u
      br_if 0 (;@1;)
      unreachable
    end
    unreachable)
  (func (;35;) (type 7) (param i32)
    (local i32)
    global.get 0
    i32.const 16
    i32.sub
    local.tee 1
    global.set 0
    local.get 0
    i32.load offset=8
    i32.eqz
    if  ;; label = @1
      i32.const 1049804
      call 17
      unreachable
    end
    local.get 1
    local.get 0
    i32.const 20
    i32.add
    i64.load align=4
    i64.store offset=8
    local.get 1
    local.get 0
    i64.load offset=12 align=4
    i64.store
    call 34
    unreachable)
  (func (;36;) (type 11) (param i64 i32) (result i32)
    (local i32 i32 i32 i32 i32 i64)
    global.get 0
    i32.const 48
    i32.sub
    local.tee 4
    global.set 0
    i32.const 39
    local.set 2
    block  ;; label = @1
      local.get 0
      i64.const 10000
      i64.lt_u
      if  ;; label = @2
        local.get 0
        local.set 7
        br 1 (;@1;)
      end
      loop  ;; label = @2
        local.get 4
        i32.const 9
        i32.add
        local.get 2
        i32.add
        local.tee 3
        i32.const -4
        i32.add
        local.get 0
        i64.const 10000
        i64.div_u
        local.tee 7
        i64.const -10000
        i64.mul
        local.get 0
        i64.add
        i32.wrap_i64
        local.tee 5
        i32.const 65535
        i32.and
        i32.const 100
        i32.div_u
        local.tee 6
        i32.const 1
        i32.shl
        i32.const 1049434
        i32.add
        i32.load16_u align=1
        i32.store16 align=1
        local.get 3
        i32.const -2
        i32.add
        local.get 6
        i32.const -100
        i32.mul
        local.get 5
        i32.add
        i32.const 65535
        i32.and
        i32.const 1
        i32.shl
        i32.const 1049434
        i32.add
        i32.load16_u align=1
        i32.store16 align=1
        local.get 2
        i32.const -4
        i32.add
        local.set 2
        local.get 0
        i64.const 99999999
        i64.gt_u
        local.get 7
        local.set 0
        br_if 0 (;@2;)
      end
    end
    local.get 7
    i32.wrap_i64
    local.tee 3
    i32.const 99
    i32.gt_s
    if  ;; label = @1
      local.get 7
      i32.wrap_i64
      local.tee 5
      i32.const 65535
      i32.and
      i32.const 100
      i32.div_u
      local.set 3
      local.get 2
      i32.const -2
      i32.add
      local.tee 2
      local.get 4
      i32.const 9
      i32.add
      i32.add
      local.get 3
      i32.const -100
      i32.mul
      local.get 5
      i32.add
      i32.const 65535
      i32.and
      i32.const 1
      i32.shl
      i32.const 1049434
      i32.add
      i32.load16_u align=1
      i32.store16 align=1
    end
    block  ;; label = @1
      local.get 3
      i32.const 10
      i32.ge_s
      if  ;; label = @2
        local.get 2
        i32.const -2
        i32.add
        local.tee 2
        local.get 4
        i32.const 9
        i32.add
        i32.add
        local.get 3
        i32.const 1
        i32.shl
        i32.const 1049434
        i32.add
        i32.load16_u align=1
        i32.store16 align=1
        br 1 (;@1;)
      end
      local.get 2
      i32.const -1
      i32.add
      local.tee 2
      local.get 4
      i32.const 9
      i32.add
      i32.add
      local.get 3
      i32.const 48
      i32.add
      i32.store8
    end
    local.get 1
    i32.const 1049634
    i32.const 0
    local.get 4
    i32.const 9
    i32.add
    local.get 2
    i32.add
    i32.const 39
    local.get 2
    i32.sub
    call 21
    local.get 4
    i32.const 48
    i32.add
    global.set 0)
  (func (;37;) (type 12) (param i32) (result i64)
    i64.const 3105409919041120946)
  (func (;38;) (type 13) (param i32 i32 i32 i32) (result i32)
    block (result i32)  ;; label = @1
      local.get 1
      i32.const 1114112
      i32.ne
      if  ;; label = @2
        i32.const 1
        local.get 0
        i32.load offset=24
        local.get 1
        local.get 0
        i32.const 28
        i32.add
        i32.load
        i32.load offset=16
        call_indirect (type 2)
        br_if 1 (;@1;)
        drop
      end
      local.get 2
      i32.eqz
      if  ;; label = @2
        i32.const 0
        return
      end
      local.get 0
      i32.load offset=24
      local.get 2
      local.get 3
      local.get 0
      i32.const 28
      i32.add
      i32.load
      i32.load offset=12
      call_indirect (type 1)
    end)
  (func (;39;) (type 1) (param i32 i32 i32) (result i32)
    (local i32 i32 i32 i32 i32 i32 i32 i32)
    global.get 0
    i32.const -64
    i32.add
    local.tee 3
    global.set 0
    local.get 3
    i32.const 36
    i32.add
    local.get 1
    i32.store
    local.get 3
    i32.const 52
    i32.add
    local.get 2
    i32.const 20
    i32.add
    i32.load
    local.tee 4
    i32.store
    local.get 3
    i32.const 3
    i32.store8 offset=56
    local.get 3
    i32.const 44
    i32.add
    local.get 2
    i32.load offset=16
    local.tee 5
    local.get 4
    i32.const 3
    i32.shl
    i32.add
    i32.store
    local.get 3
    i64.const 137438953472
    i64.store offset=8
    local.get 3
    local.get 0
    i32.store offset=32
    local.get 3
    i32.const 0
    i32.store offset=24
    local.get 3
    i32.const 0
    i32.store offset=16
    local.get 3
    local.get 5
    i32.store offset=48
    local.get 3
    local.get 5
    i32.store offset=40
    block  ;; label = @1
      block  ;; label = @2
        block  ;; label = @3
          block  ;; label = @4
            local.get 2
            i32.load offset=8
            local.tee 6
            i32.eqz
            if  ;; label = @5
              local.get 2
              i32.load
              local.set 8
              local.get 2
              i32.load offset=4
              local.tee 9
              local.get 4
              local.get 4
              local.get 9
              i32.gt_u
              select
              local.tee 10
              i32.eqz
              br_if 1 (;@4;)
              i32.const 1
              local.set 4
              local.get 0
              local.get 8
              i32.load
              local.get 8
              i32.load offset=4
              local.get 1
              i32.load offset=12
              call_indirect (type 1)
              br_if 4 (;@1;)
              local.get 8
              i32.const 8
              i32.add
              local.set 2
              i32.const 1
              local.set 7
              loop  ;; label = @6
                local.get 5
                i32.load
                local.get 3
                i32.const 8
                i32.add
                local.get 5
                i32.const 4
                i32.add
                i32.load
                call_indirect (type 2)
                br_if 5 (;@1;)
                local.get 7
                local.get 10
                i32.ge_u
                br_if 2 (;@4;)
                local.get 2
                i32.const 4
                i32.add
                local.set 0
                local.get 2
                i32.load
                local.set 1
                local.get 5
                i32.const 8
                i32.add
                local.set 5
                local.get 2
                i32.const 8
                i32.add
                local.set 2
                local.get 7
                i32.const 1
                i32.add
                local.set 7
                local.get 3
                i32.load offset=32
                local.get 1
                local.get 0
                i32.load
                local.get 3
                i32.load offset=36
                i32.load offset=12
                call_indirect (type 1)
                i32.eqz
                br_if 0 (;@6;)
              end
              br 4 (;@1;)
            end
            local.get 2
            i32.load
            local.set 8
            local.get 2
            i32.load offset=4
            local.tee 9
            local.get 2
            i32.const 12
            i32.add
            i32.load
            local.tee 5
            local.get 5
            local.get 9
            i32.gt_u
            select
            local.tee 10
            i32.eqz
            br_if 0 (;@4;)
            i32.const 1
            local.set 4
            local.get 0
            local.get 8
            i32.load
            local.get 8
            i32.load offset=4
            local.get 1
            i32.load offset=12
            call_indirect (type 1)
            br_if 3 (;@1;)
            local.get 6
            i32.const 16
            i32.add
            local.set 5
            local.get 8
            i32.const 8
            i32.add
            local.set 2
            i32.const 1
            local.set 7
            loop  ;; label = @5
              local.get 3
              local.get 5
              i32.const -8
              i32.add
              i32.load
              i32.store offset=12
              local.get 3
              local.get 5
              i32.const 16
              i32.add
              i32.load8_u
              i32.store8 offset=56
              local.get 3
              local.get 5
              i32.const -4
              i32.add
              i32.load
              i32.store offset=8
              i32.const 0
              local.set 1
              i32.const 0
              local.set 4
              block  ;; label = @6
                block  ;; label = @7
                  block  ;; label = @8
                    block  ;; label = @9
                      local.get 5
                      i32.const 8
                      i32.add
                      i32.load
                      i32.const 1
                      i32.sub
                      br_table 1 (;@8;) 2 (;@7;) 3 (;@6;) 0 (;@9;)
                    end
                    local.get 5
                    i32.const 12
                    i32.add
                    i32.load
                    local.set 0
                    i32.const 1
                    local.set 4
                    br 2 (;@6;)
                  end
                  local.get 5
                  i32.const 12
                  i32.add
                  i32.load
                  local.tee 6
                  local.get 3
                  i32.load offset=52
                  local.tee 4
                  i32.lt_u
                  if  ;; label = @8
                    i32.const 0
                    local.set 4
                    local.get 3
                    i32.load offset=48
                    local.get 6
                    i32.const 3
                    i32.shl
                    i32.add
                    local.tee 6
                    i32.load offset=4
                    i32.const 5
                    i32.ne
                    br_if 2 (;@6;)
                    local.get 6
                    i32.load
                    i32.load
                    local.set 0
                    i32.const 1
                    local.set 4
                    br 2 (;@6;)
                  end
                  i32.const 1049828
                  local.get 6
                  local.get 4
                  call 19
                  unreachable
                end
                local.get 3
                i32.load offset=40
                local.tee 6
                local.get 3
                i32.load offset=44
                i32.eq
                br_if 0 (;@6;)
                local.get 3
                local.get 6
                i32.const 8
                i32.add
                i32.store offset=40
                local.get 6
                i32.load offset=4
                i32.const 5
                i32.ne
                br_if 0 (;@6;)
                local.get 6
                i32.load
                i32.load
                local.set 0
                i32.const 1
                local.set 4
              end
              local.get 3
              local.get 0
              i32.store offset=20
              local.get 3
              local.get 4
              i32.store offset=16
              block  ;; label = @6
                block (result i32)  ;; label = @7
                  block  ;; label = @8
                    block  ;; label = @9
                      block  ;; label = @10
                        block  ;; label = @11
                          block  ;; label = @12
                            local.get 5
                            i32.load
                            i32.const 1
                            i32.sub
                            br_table 1 (;@11;) 0 (;@12;) 6 (;@6;) 4 (;@8;)
                          end
                          local.get 3
                          i32.load offset=40
                          local.tee 0
                          local.get 3
                          i32.load offset=44
                          i32.ne
                          br_if 1 (;@10;)
                          br 5 (;@6;)
                        end
                        local.get 5
                        i32.const 4
                        i32.add
                        i32.load
                        local.tee 0
                        local.get 3
                        i32.load offset=52
                        local.tee 4
                        i32.ge_u
                        br_if 1 (;@9;)
                        local.get 3
                        i32.load offset=48
                        local.get 0
                        i32.const 3
                        i32.shl
                        i32.add
                        local.tee 0
                        i32.load offset=4
                        i32.const 5
                        i32.ne
                        br_if 4 (;@6;)
                        local.get 0
                        i32.load
                        i32.load
                        br 3 (;@7;)
                      end
                      local.get 3
                      local.get 0
                      i32.const 8
                      i32.add
                      i32.store offset=40
                      local.get 0
                      i32.load offset=4
                      i32.const 5
                      i32.ne
                      br_if 3 (;@6;)
                      local.get 0
                      i32.load
                      i32.load
                      br 2 (;@7;)
                    end
                    i32.const 1049828
                    local.get 0
                    local.get 4
                    call 19
                    unreachable
                  end
                  local.get 5
                  i32.const 4
                  i32.add
                  i32.load
                end
                local.set 4
                i32.const 1
                local.set 1
              end
              local.get 3
              local.get 4
              i32.store offset=28
              local.get 3
              local.get 1
              i32.store offset=24
              block  ;; label = @6
                local.get 5
                i32.const -16
                i32.add
                i32.load
                i32.const 1
                i32.ne
                if  ;; label = @7
                  local.get 3
                  i32.load offset=40
                  local.tee 4
                  local.get 3
                  i32.load offset=44
                  i32.eq
                  br_if 4 (;@3;)
                  local.get 3
                  local.get 4
                  i32.const 8
                  i32.add
                  i32.store offset=40
                  br 1 (;@6;)
                end
                local.get 5
                i32.const -12
                i32.add
                i32.load
                local.tee 4
                local.get 3
                i32.load offset=52
                local.tee 0
                i32.ge_u
                br_if 4 (;@2;)
                local.get 3
                i32.load offset=48
                local.get 4
                i32.const 3
                i32.shl
                i32.add
                local.set 4
              end
              local.get 4
              i32.load
              local.get 3
              i32.const 8
              i32.add
              local.get 4
              i32.const 4
              i32.add
              i32.load
              call_indirect (type 2)
              if  ;; label = @6
                i32.const 1
                local.set 4
                br 5 (;@1;)
              end
              local.get 7
              local.get 10
              i32.ge_u
              br_if 1 (;@4;)
              local.get 2
              i32.const 4
              i32.add
              local.set 0
              local.get 2
              i32.load
              local.set 1
              local.get 5
              i32.const 36
              i32.add
              local.set 5
              local.get 2
              i32.const 8
              i32.add
              local.set 2
              i32.const 1
              local.set 4
              local.get 7
              i32.const 1
              i32.add
              local.set 7
              local.get 3
              i32.load offset=32
              local.get 1
              local.get 0
              i32.load
              local.get 3
              i32.load offset=36
              i32.load offset=12
              call_indirect (type 1)
              i32.eqz
              br_if 0 (;@5;)
            end
            br 3 (;@1;)
          end
          local.get 9
          local.get 7
          i32.gt_u
          if  ;; label = @4
            i32.const 1
            local.set 4
            local.get 3
            i32.load offset=32
            local.get 7
            i32.const 3
            i32.shl
            local.get 8
            i32.add
            local.tee 5
            i32.load
            local.get 5
            i32.load offset=4
            local.get 3
            i32.load offset=36
            i32.load offset=12
            call_indirect (type 1)
            br_if 3 (;@1;)
          end
          i32.const 0
          local.set 4
          br 2 (;@1;)
        end
        i32.const 1049804
        call 17
        unreachable
      end
      i32.const 1049844
      local.get 4
      local.get 0
      call 19
      unreachable
    end
    local.get 3
    i32.const -64
    i32.sub
    global.set 0
    local.get 4)
  (func (;40;) (type 10) (param i32) (result i32)
    (local i32 i32)
    i32.const 204
    local.set 2
    local.get 0
    local.set 1
    loop  ;; label = @1
      local.get 1
      i32.const 0
      i32.store8
      local.get 1
      i32.const 1
      i32.add
      local.set 1
      local.get 2
      i32.const -1
      i32.add
      local.tee 2
      br_if 0 (;@1;)
    end
    local.get 0)
  (func (;41;) (type 1) (param i32 i32 i32) (result i32)
    (local i32)
    local.get 2
    if  ;; label = @1
      local.get 0
      local.set 3
      loop  ;; label = @2
        local.get 3
        local.get 1
        i32.load8_u
        i32.store8
        local.get 3
        i32.const 1
        i32.add
        local.set 3
        local.get 1
        i32.const 1
        i32.add
        local.set 1
        local.get 2
        i32.const -1
        i32.add
        local.tee 2
        br_if 0 (;@2;)
      end
    end
    local.get 0)
  (table (;0;) 17 17 anyfunc)
  (memory (;0;) 17)
  (global (;0;) (mut i32) (i32.const 1048576))
  (export "memory" (memory 0))
  (export "main" (func 4))
  (elem (;0;) (i32.const 1) 22 20 13 14 20 26 29 30 31 26 24 27 28 26 26 37)
  (data (;0;) (i32.const 1048576) "\18\00\10\00(\00\00\00@\00\10\00Y\00\00\00\9e\00\00\00\0d\00\00\00assertion failed: dst.len() <= src.len()/home/ubuntu/.cargo/registry/src/github.com-1ecc6299db9ec823/tiny-keccak-1.5.0/src/lib.rs\00\00\00\b0\00\10\00Y\00\00\00\ad\00\00\00(\00\00\00\00\00\00\00/home/ubuntu/.cargo/registry/src/github.com-1ecc6299db9ec823/tiny-keccak-1.5.0/src/lib.rs\00\00\00\b0\00\10\00Y\00\00\00\ae\00\00\00*\00\00\00\00\00\00\00\01\00\00\00\00\00\00\00\82\80\00\00\00\00\00\00\8a\80\00\00\00\00\00\80\00\80\00\80\00\00\00\80\8b\80\00\00\00\00\00\00\01\00\00\80\00\00\00\00\81\80\00\80\00\00\00\80\09\80\00\00\00\00\00\80\8a\00\00\00\00\00\00\00\88\00\00\00\00\00\00\00\09\80\00\80\00\00\00\00\0a\00\00\80\00\00\00\00\8b\80\00\80\00\00\00\00\8b\00\00\00\00\00\00\80\89\80\00\00\00\00\00\80\03\80\00\00\00\00\00\80\02\80\00\00\00\00\00\80\80\00\00\00\00\00\00\80\0a\80\00\00\00\00\00\00\0a\00\00\80\00\00\00\80\81\80\00\80\00\00\00\80\80\80\00\00\00\00\00\80\01\00\00\80\00\00\00\00\08\80\00\80\00\00\00\80\f8\01\10\00-\00\00\00%\02\10\00\0c\00\00\001\02\10\00\03\00\00\00assertion failed: `(left == right)`\0a  left: ``,\0a right: ``: <\02\10\004\00\00\00destination and source slices have different lengthsD\04\10\00\18\00\00\00X\08\00\00\09\00\00\00\06\00\00\00\00\00\00\00\01\00\00\00\07\00\00\00\08\00\00\00\09\00\00\00\0a\00\00\00\04\00\00\00\04\00\00\00\0b\00\00\00\0c\00\00\00\0d\00\00\00\0e\00\00\00\00\00\00\00\01\00\00\00\07\00\00\00\08\00\00\00\09\00\00\00\e0\02\10\00\11\00\00\00\f1\02\10\00\17\00\00\00\f2\02\00\00\05\00\00\00capacity overflowsrc/liballoc/raw_vec.rs(\03\10\00 \00\00\00H\03\10\00\12\00\00\00\0f\00\00\00\00\00\00\00\01\00\00\00\10\00\00\00index out of bounds: the len is  but the index is 00010203040506070809101112131415161718192021222324252627282930313233343536373839404142434445464748495051525354555657585960616263646566676869707172737475767778798081828384858687888990919293949596979899\00\00\5c\04\10\00\06\00\00\00b\04\10\00\22\00\00\00D\04\10\00\18\00\00\00\17\0a\00\00\05\00\00\00src/libcore/slice/mod.rsindex  out of range for slice of length \a4\04\10\00\16\00\00\00\ba\04\10\00\0d\00\00\00D\04\10\00\18\00\00\00\1d\0a\00\00\05\00\00\00slice index starts at  but ends at 0x\00\00\00&\05\10\00+\00\00\00Q\05\10\00\15\00\00\00z\01\00\00\15\00\00\00\10\05\10\00\16\00\00\00a\04\00\00\11\00\00\00\10\05\10\00\16\00\00\00U\04\00\00(")
  (data (;1;) (i32.const 1049872) "src/libcore/fmt/mod.rscalled `Option::unwrap()` on a `None` valuesrc/libcore/option.rs"))
