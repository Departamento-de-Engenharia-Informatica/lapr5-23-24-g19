import { assert } from 'chai'
import { describe, it } from 'mocha'
import { TaskType } from './taskType'

describe('TaskType', () => {
    it('should create a valid task type', () => {
        const validTaskTypeValue = 'Test Task Type'
        const taskType = TaskType.create({ value: validTaskTypeValue })

        assert.isOk(taskType.isSuccess)
    })

    it('should fail when creating a task type with null or undefined value', () => {
        const taskType1 = TaskType.create({ value: null })
        assert.isNotOk(taskType1.isSuccess)

        const taskType2 = TaskType.create({ value: undefined })
        assert.isNotOk(taskType2.isSuccess)
    })

    it('should trim leading and trailing whitespace from the value', () => {
        const taskTypeValue = '   Leading and Trailing Whitespace   '
        const taskType = TaskType.create({ value: taskTypeValue })

        assert.isOk(taskType.isSuccess)
        assert.equal(taskType.getValue().value, 'Leading and Trailing Whitespace')
    })
})
