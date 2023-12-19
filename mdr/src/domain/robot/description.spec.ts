import { assert } from 'chai'
import { describe, it } from 'mocha'

import { RobotDescription } from './description'

describe('Elevator Description', () => {
    it('must be alphanumeric', () => {
        let desc = RobotDescription.create('I am not a robot, I am human 🤖')
        assert.isNotOk(desc.isSuccess)

        desc = RobotDescription.create('#1 Robot in the world!')
        assert.isNotOk(desc.isSuccess)

        desc = RobotDescription.create('ILoveRobots')
        assert.isOk(desc.isSuccess)
    })

    it('can contain spaces', () => {
        let desc = RobotDescription.create('Number 1 Robot in the world')
        assert.isOk(desc.isSuccess)

        desc = RobotDescription.create('Robot Xpto')
        assert.isOk(desc.isSuccess)
    })
})
