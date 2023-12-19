import { assert } from 'chai'
import { describe, it } from 'mocha'
import { Passage } from './passage'

describe('Passage', () => {
    it('floors cannot be undefined', () => {
        let result = Passage.create({
            floor1: undefined,
            floor2: undefined,
        })

        assert(result.isFailure, 'Undefined floor values should fail')
    })
})
