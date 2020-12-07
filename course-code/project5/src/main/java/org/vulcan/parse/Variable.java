package org.vulcan.parse;

import org.vulcan.TypeChecker.JType;

import java.util.Objects;

import static org.vulcan.parse.TokenType.VAR;

/**
 * Jam variable class
 * @author Think
 */
public class Variable implements Token, Term {
    private final String name;

    /**
     * Instantiates a new org.vulcan.parse.Variable.
     *
     * @param n the n
     */
    Variable(final String n) {
        this.name = n;
    }

    /**
     * Gets name.
     *
     * @return the name
     */
    public String getName() {
        return this.name;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        Variable variable = (Variable) o;
        return Objects.equals(name, variable.name);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name);
    }

    @Override
    public <T> T accept(final AstVisitor<T> v) {
        return v.forVariable(this);
    }

    @Override
    public String toString() {
        return this.name;
    }


    @Override
    public TokenType getType() {
        return VAR;
    }
}
