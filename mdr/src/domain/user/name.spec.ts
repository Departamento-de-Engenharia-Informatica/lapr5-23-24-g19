import { assert } from 'chai'
import { describe, it } from 'mocha'

import { Name } from './name'

describe('Client Name', () => {
    it('cannot be null', () => {
        const name = Name.create(null)
        assert.isNotOk(name.isSuccess)
    })

    it('cannot be undefined', () => {
        const name = Name.create(undefined)
        assert.isNotOk(name.isSuccess)
    })

    it('allows blanks', () => {
        const name = Name.create('    ')
        assert.isOk(name.isSuccess)
    })
})
