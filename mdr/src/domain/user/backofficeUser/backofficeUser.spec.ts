import { assert } from 'chai'
import { createSandbox } from 'sinon'
import { describe, it } from 'mocha'

import { Result } from '../../../core/logic/Result'
import { PhoneNumber } from '../phoneNumber'
import { UserPassword } from '../userPassword'
import { Email } from '../email'
import { Name } from '../name'
import BackofficeUser from './backofficeUser'
import { Role } from '../../role'

describe('BackofficeUser create', () => {
    const sinon = createSandbox()

    function stubCreate<K>(klass: K) {
        sinon.stub(klass, 'create' as keyof K).returns(Result.ok<K>({} as K))
    }

    afterEach(sinon.restore)

    it('should fail when creating a backoffice user with null or undefined email', () => {
        //stubCreate(Email)
        stubCreate(Name)
        stubCreate(PhoneNumber)
        stubCreate(UserPassword)
        stubCreate(Role)

        const result = BackofficeUser.create({
            email: undefined,
            role: Role.create({ name: 'Task Manager', active: true }).getValue(),
            name: Name.create('').getValue(),
            phoneNumber: PhoneNumber.create(0).getValue(),
            password: UserPassword.create({ value: '', hashed: false }).getValue(),
        })

        assert.isNotOk(result.isSuccess)
    })

    it('should fail when creating a backoffice user with null or undefined name', () => {
        stubCreate(Email)
        //stubCreate(Name)
        stubCreate(PhoneNumber)
        stubCreate(UserPassword)

        const result = BackofficeUser.create({
            email: Email.create('').getValue(),
            role: Role.create({ name: 'Task Manager', active: true }).getValue(),
            name: undefined,
            phoneNumber: PhoneNumber.create(0).getValue(),
            password: UserPassword.create({ value: '', hashed: false }).getValue(),
        })

        assert.isNotOk(result.isSuccess)
    })

    it('should fail when creating a backoffice user with null or undefined phoneNumber', () => {
        stubCreate(Email)
        stubCreate(Name)
        //stubCreate(PhoneNumber)
        stubCreate(UserPassword)

        const result = BackofficeUser.create({
            email: Email.create('').getValue(),
            role: Role.create({ name: 'Task Manager', active: true }).getValue(),
            name: Name.create('').getValue(),
            phoneNumber: undefined,
            password: UserPassword.create({ value: '', hashed: false }).getValue(),
        })

        assert.isNotOk(result.isSuccess)
    })

    it('should fail when creating a backoffice user with null or undefined password', () => {
        stubCreate(Email)
        stubCreate(Name)
        stubCreate(PhoneNumber)
        //stubCreate(UserPassword)

        const result = BackofficeUser.create({
            email: Email.create('').getValue(),
            role: Role.create({ name: 'Task Manager', active: true }).getValue(),
            name: Name.create('').getValue(),
            phoneNumber: PhoneNumber.create(0).getValue(),
            password: undefined,
        })

        assert.isNotOk(result.isSuccess)
    })
})
