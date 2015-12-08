--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: lesson; Type: TABLE; Schema: public; Owner: flux; Tablespace: 
--

CREATE TABLE lesson (
    id integer NOT NULL,
    keyphrase character varying NOT NULL,
    jsfile character varying NOT NULL,
    title character varying NOT NULL,
    goon character varying NOT NULL
);


ALTER TABLE lesson OWNER TO flux;

--
-- Name: lesson_completed; Type: TABLE; Schema: public; Owner: flux; Tablespace: 
--

CREATE TABLE lesson_completed (
    id integer NOT NULL,
    "user" bigint NOT NULL,
    lesson bigint NOT NULL
);


ALTER TABLE lesson_completed OWNER TO flux;

--
-- Name: lesson_completed_id_seq; Type: SEQUENCE; Schema: public; Owner: flux
--

CREATE SEQUENCE lesson_completed_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE lesson_completed_id_seq OWNER TO flux;

--
-- Name: lesson_completed_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: flux
--

ALTER SEQUENCE lesson_completed_id_seq OWNED BY lesson_completed.id;


--
-- Name: lesson_id_seq; Type: SEQUENCE; Schema: public; Owner: flux
--

CREATE SEQUENCE lesson_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE lesson_id_seq OWNER TO flux;

--
-- Name: lesson_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: flux
--

ALTER SEQUENCE lesson_id_seq OWNED BY lesson.id;


--
-- Name: lesson_prereqs; Type: TABLE; Schema: public; Owner: flux; Tablespace: 
--

CREATE TABLE lesson_prereqs (
    id integer NOT NULL,
    lesson_for bigint NOT NULL,
    lesson_required bigint NOT NULL
);


ALTER TABLE lesson_prereqs OWNER TO flux;

--
-- Name: lesson_prereqs_id_seq; Type: SEQUENCE; Schema: public; Owner: flux
--

CREATE SEQUENCE lesson_prereqs_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE lesson_prereqs_id_seq OWNER TO flux;

--
-- Name: lesson_prereqs_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: flux
--

ALTER SEQUENCE lesson_prereqs_id_seq OWNED BY lesson_prereqs.id;


--
-- Name: user; Type: TABLE; Schema: public; Owner: flux; Tablespace: 
--

CREATE TABLE "user" (
    id integer NOT NULL,
    name character varying NOT NULL,
    email character varying NOT NULL
);


ALTER TABLE "user" OWNER TO flux;

--
-- Name: user_id_seq; Type: SEQUENCE; Schema: public; Owner: flux
--

CREATE SEQUENCE user_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE user_id_seq OWNER TO flux;

--
-- Name: user_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: flux
--

ALTER SEQUENCE user_id_seq OWNED BY "user".id;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: flux
--

ALTER TABLE ONLY lesson ALTER COLUMN id SET DEFAULT nextval('lesson_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: flux
--

ALTER TABLE ONLY lesson_completed ALTER COLUMN id SET DEFAULT nextval('lesson_completed_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: flux
--

ALTER TABLE ONLY lesson_prereqs ALTER COLUMN id SET DEFAULT nextval('lesson_prereqs_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: flux
--

ALTER TABLE ONLY "user" ALTER COLUMN id SET DEFAULT nextval('user_id_seq'::regclass);


--
-- Data for Name: lesson; Type: TABLE DATA; Schema: public; Owner: flux
--

COPY lesson (id, keyphrase, jsfile, title, goon) FROM stdin;
1	Addition is a way of combining things.	lesson1	Addition as Combination	Why are you reading this?
3	Sometimes we have the answer and need the question.	lesson3	Variable in the Middle	This is boring. What else can we do?
4	Some questions can be answered together.	lesson4	Multiple Equations	Can we reuse that X?
5	Some questions must be answered together.	lesson5	Multiple Variables	What happens if we add a Y?
2	An X doesn't change anything.	lesson2	Adding a variable	How else can we write a blank?
6	Addition run backwards is subtraction.	lesson6	Subtraction as Inverse	Can we run that in reverse?
\.


--
-- Data for Name: lesson_completed; Type: TABLE DATA; Schema: public; Owner: flux
--

COPY lesson_completed (id, "user", lesson) FROM stdin;
1	3	1
2	3	2
3	3	3
4	4	1
\.


--
-- Name: lesson_completed_id_seq; Type: SEQUENCE SET; Schema: public; Owner: flux
--

SELECT pg_catalog.setval('lesson_completed_id_seq', 4, true);


--
-- Name: lesson_id_seq; Type: SEQUENCE SET; Schema: public; Owner: flux
--

SELECT pg_catalog.setval('lesson_id_seq', 6, true);


--
-- Data for Name: lesson_prereqs; Type: TABLE DATA; Schema: public; Owner: flux
--

COPY lesson_prereqs (id, lesson_for, lesson_required) FROM stdin;
6	2	1
7	3	2
8	4	3
9	5	4
10	6	3
\.


--
-- Name: lesson_prereqs_id_seq; Type: SEQUENCE SET; Schema: public; Owner: flux
--

SELECT pg_catalog.setval('lesson_prereqs_id_seq', 10, true);


--
-- Data for Name: user; Type: TABLE DATA; Schema: public; Owner: flux
--

COPY "user" (id, name, email) FROM stdin;
3	nomicflux	nomicflux@gmail.com
4	mjanderson235	mjanderson235@gmail.com
\.


--
-- Name: user_id_seq; Type: SEQUENCE SET; Schema: public; Owner: flux
--

SELECT pg_catalog.setval('user_id_seq', 4, true);


--
-- Name: lesson_completed_pkey; Type: CONSTRAINT; Schema: public; Owner: flux; Tablespace: 
--

ALTER TABLE ONLY lesson_completed
    ADD CONSTRAINT lesson_completed_pkey PRIMARY KEY (id);


--
-- Name: lesson_pkey; Type: CONSTRAINT; Schema: public; Owner: flux; Tablespace: 
--

ALTER TABLE ONLY lesson
    ADD CONSTRAINT lesson_pkey PRIMARY KEY (id);


--
-- Name: lesson_prereqs_pkey; Type: CONSTRAINT; Schema: public; Owner: flux; Tablespace: 
--

ALTER TABLE ONLY lesson_prereqs
    ADD CONSTRAINT lesson_prereqs_pkey PRIMARY KEY (id);


--
-- Name: unique_user; Type: CONSTRAINT; Schema: public; Owner: flux; Tablespace: 
--

ALTER TABLE ONLY "user"
    ADD CONSTRAINT unique_user UNIQUE (email);


--
-- Name: user_pkey; Type: CONSTRAINT; Schema: public; Owner: flux; Tablespace: 
--

ALTER TABLE ONLY "user"
    ADD CONSTRAINT user_pkey PRIMARY KEY (id);


--
-- Name: lesson_completed_lesson_fkey; Type: FK CONSTRAINT; Schema: public; Owner: flux
--

ALTER TABLE ONLY lesson_completed
    ADD CONSTRAINT lesson_completed_lesson_fkey FOREIGN KEY (lesson) REFERENCES lesson(id);


--
-- Name: lesson_completed_user_fkey; Type: FK CONSTRAINT; Schema: public; Owner: flux
--

ALTER TABLE ONLY lesson_completed
    ADD CONSTRAINT lesson_completed_user_fkey FOREIGN KEY ("user") REFERENCES lesson(id);


--
-- Name: lesson_prereqs_lesson_for_fkey; Type: FK CONSTRAINT; Schema: public; Owner: flux
--

ALTER TABLE ONLY lesson_prereqs
    ADD CONSTRAINT lesson_prereqs_lesson_for_fkey FOREIGN KEY (lesson_for) REFERENCES lesson(id);


--
-- Name: lesson_prereqs_lesson_required_fkey; Type: FK CONSTRAINT; Schema: public; Owner: flux
--

ALTER TABLE ONLY lesson_prereqs
    ADD CONSTRAINT lesson_prereqs_lesson_required_fkey FOREIGN KEY (lesson_required) REFERENCES lesson(id);


--
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--

