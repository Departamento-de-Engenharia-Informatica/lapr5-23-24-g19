function loginViaAuth0Ui(username: string, password: string) {
    cy.origin(
        Cypress.env('auth_domain'),
        { args: { username, password } },
        ({ username, password }) => {
            cy.get('input#1-email').type(username)
            cy.get('input#1-password').type(password, { log: false })
            cy.get('button[type="submit"]')
                .should('be.visible')
                .should('contain.text', 'Log In')
                .should('not.be.disabled')
                .should('not.be.hidden')
                .click()
        },
    )
}

describe('CreateRobotTypeComponent e2e tests', () => {
    beforeEach(() => {
        cy.intercept('GET', 'http://localhost:4000/api/task/types', {
            body: [{ description: 'Task 1' }, { description: 'Task 2' }],
        }).as('getTaskTypes')
        window.localStorage.setItem('USER_ROLES', 'ADM')

        cy.visit('/fleet/robot-types/create')
        const log = Cypress.log({
            displayName: 'AUTH0 LOGIN',
            message: [`🔐 Authenticating | ${Cypress.env('auth_username')}`],
            // @ts-ignore
            autoEnd: false,
        })
        log.snapshot('before')

        loginViaAuth0Ui(Cypress.env('auth_username'), Cypress.env('auth_password'))

        log.snapshot('after')
        log.end()

        cy.wait('@getTaskTypes')
    })

    it('should display the form with required fields', () => {
        cy.get('#code').should('exist')
        cy.get('#brand').should('exist')
        cy.get('#model').should('exist')
        cy.get('#taskTypes').should('exist')
        cy.get('button[type="submit"]').should('be.disabled')
    })

    it('should enable button when everything is filled', () => {
        cy.get('button[type="submit"]').should('be.disabled')
        cy.get('#code').type('ABC')
        cy.get('button[type="submit"]').should('be.disabled')
        cy.get('#brand').type('Brand')
        cy.get('button[type="submit"]').should('be.disabled')
        cy.get('#model').type('Model')
        cy.get('button[type="submit"]').should('be.disabled')
        cy.get('#taskTypes').select(['Task 1', 'Task 2'])
        cy.get('button[type="submit"]').should('be.enabled')
    })

    it('should create a robot type successfully', () => {
        cy.get('#code').type('ABC')
        cy.get('#brand').type('Brand')
        cy.get('#model').type('Model')
        cy.get('#taskTypes').select(['Task 1', 'Task 2'])

        cy.get('button[type="submit"]').click()
    })

    it('should reset the form after successful submission', () => {
        cy.intercept('POST', 'http://localhost:4000/api/robottypes', {
            statusCode: 201,
            body: 'Robot type created successfully',
        }).as('createRobotType')

        cy.get('#code').type('ABC')
        cy.get('#brand').type('Brand')
        cy.get('#model').type('Model')
        cy.get('#taskTypes').select(['Task 1', 'Task 2'])

        cy.get('button[type="submit"]').click()

        cy.wait('@createRobotType')
        cy.on('window:alert', (text) => {
            expect(text).to.exist
        })

        cy.get('#code').should('have.value', '')
        cy.get('#brand').should('have.value', '')
        cy.get('#model').should('have.value', '')
        cy.get('#taskTypes').find('option:selected').should('have.length', 0)
    })
})
