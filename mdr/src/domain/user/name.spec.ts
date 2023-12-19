import { assert } from 'chai'
import { describe, it } from 'mocha'

import { Name } from './name'

describe('Client Name', () => {
    it('can have any type of characters and size', () => {
        const name = Name.create('nameofclient')
        assert.isOk(name.isSuccess)
    })

    it('allows blanks', () => {
        const name = Name.create('    ')
        assert.isOk(name.isSuccess)
    })
})
