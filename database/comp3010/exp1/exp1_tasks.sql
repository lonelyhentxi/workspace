CREATE TABLE `employee`
(
  `ename`    VARCHAR(40)   NULL,
  `essn`     VARCHAR(40)   NOT NULL,
  `address`  VARCHAR(40)   NULL,
  `salary`   DECIMAL(7, 2) NULL,
  `superssn` VARCHAR(40)   NULL,
  `dno`      VARCHAR(40)   NULL,
  CONSTRAINT department_pk
    PRIMARY KEY (`essn`)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4;

CREATE TABLE department
(
  dname        VARCHAR(40) NULL,
  dno          VARCHAR(40) NOT NULL,
  mgrssn       VARCHAR(40) NULL,
  mgrstartdate DATETIME    NULL,
  CONSTRAINT department_pk
    PRIMARY KEY (dno)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4;

CREATE TABLE project
(
  pname     VARCHAR(40) NULL,
  pno       VARCHAR(40) NOT NULL,
  plocation VARCHAR(40) NULL,
  dno       VARCHAR(40) NULL,
  CONSTRAINT project_pk
    PRIMARY KEY (pno)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4;


CREATE TABLE works_on
(
  essn  VARCHAR(40) NOT NULL,
  pno   VARCHAR(40) NOT NULL,
  hours INT         NULL,
  CONSTRAINT works_on_pk
    PRIMARY KEY (essn, pno)
) ENGINE = InnoDB
  DEFAULT CHARSET = utf8mb4;

SELECT DISTINCT employee.ename
FROM project,
     works_on,
     employee
WHERE project.pname = 'SQL Project'
  AND project.pno = works_on.pno
  AND employee.essn = works_on.essn;

SELECT employee.ename, employee.address
FROM department,
     employee
WHERE department.dname = 'Research Department'
  AND department.dno = employee.dno
  AND employee.salary < 3000;

SELECT employee.ename
FROM employee
WHERE employee.essn
        NOT IN (
        SELECT essn as essn
        FROM works_on
        WHERE pno = 'P1'
      );

SELECT DISTINCT ename, dname
FROM employee
       NATURAL JOIN department
WHERE superssn = (
  SELECT essn
  FROM employee
  WHERE ename = '张红'
  );

SELECT essn
FROM (
       (SELECT essn
        FROM works_on
        WHERE pno = 'P1')
       UNION ALL
       (
         SELECT essn
         FROM works_on
         WHERE pno = 'P2'
       )
     )
       AS union_essn
GROUP BY (essn)
HAVING COUNT(essn) >= 2;

SELECT DISTINCT employee.essn, employee.ename
FROM employee,
     (
       SELECT works_on.essn
       FROM works_on
       GROUP BY works_on.essn
       HAVING COUNT(works_on.pno) = (
         SELECT COUNT(project.pno)
         FROM project
       )
     ) AS selected_employ_essn
WHERE employee.essn = selected_employ_essn.essn;


SELECT DISTINCT department.dname
FROM department,
     (
       SELECT employee.dno
       FROM employee
       GROUP BY (employee.dno)
       HAVING AVG(employee.salary) < 4000
     ) AS dep_avg_salary
WHERE department.dno = dep_avg_salary.dno;


SELECT DISTINCT employee.essn
FROM employee,
     (
       SELECT works_on.essn
       FROM works_on
       GROUP BY works_on.essn
       HAVING COUNT(works_on.pno) >= 3
          AND SUM(works_on.hours) <= 10
     ) AS selected_employee
WHERE employee.essn = selected_employee.essn;

SELECT department.dname, SUM(employee_info.salary) / SUM(sum_hours)
FROM department,
     (
       SELECT employee.dno, works_on.essn, employee.salary, SUM(works_on.hours) as sum_hours
       FROM works_on,
            employee
       WHERE works_on.essn = employee.essn
       GROUP BY works_on.essn, employee.dno, employee.salary
     ) AS employee_info
WHERE employee_info.dno = department.dno
GROUP BY employee_info.dno, department.dno;