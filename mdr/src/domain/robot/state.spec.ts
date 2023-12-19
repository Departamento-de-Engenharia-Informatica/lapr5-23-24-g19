import { assert } from 'chai'
import { describe, it } from 'mocha'
import { RobotState } from './state'

describe('RobotState', () => {
    it('should convert enum to string', () => {
        assert.equal(RobotState.toString(RobotState.ENABLED), 'Enabled')
        assert.equal(RobotState.toString(RobotState.DISABLED), 'Disabled')
    })

    it('should handle unknown enum value', () => {
        assert.equal(RobotState.toString(3 as RobotState), 'Unknown')
    })
})
