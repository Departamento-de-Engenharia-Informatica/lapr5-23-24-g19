import { assert } from 'chai'
import { describe, it } from 'mocha'

import { ElevatorDescription } from './description'

describe('Elevator Description', () => {
    // TODO: test the character limit maybe?

    it('must be alphanumeric', () => {
        let desc = ElevatorDescription.create('🔥🔥🔥 Best Elevator 🔥🔥🔥')
        assert.isNotOk(desc.isSuccess)

        desc = ElevatorDescription.create('#1 Elevator in the world!')
        assert.isNotOk(desc.isSuccess)

        desc = ElevatorDescription.create('ElevadorXpto')
        assert.isOk(desc.isSuccess)
    })
})
